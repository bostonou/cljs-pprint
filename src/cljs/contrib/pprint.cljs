(ns cljs.contrib.pprint
  (:refer-clojure :exclude [deftype])
  (:require
    [clojure.string :as string])
  (:require-macros
    [clj.contrib.pprint :refer [deftype getf setf pprint-logical-block print-length-loop
                                with-pretty-writer defdirectives formatter formatter-out]])
  (:import goog.string.StringBuffer))

(def ^:dynamic *out* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cljs utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- print* [& more]
  (-write *out* (apply print-str more)))

(defn- println* [& more]
  (apply print* more)
  (-write *out* \newline))

(defn- ^:dynamic pr* [& more]
  (-write *out* (apply pr-str more)))

(defn- prn* [& more]
  (apply pr* more)
  (-write *out* \newline))

(defn ^boolean float?
  "Returns true if n is an float."
  [n]
  (and (number? n)
       (not ^boolean (js/isNaN n))
       (not (identical? n js/Infinity))
       (not (== (js/parseFloat n) (js/parseInt n 10)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pprint/utilities.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- map-passing-context [func initial-context lis]
  (loop [context initial-context
         lis lis
         acc []]
    (if (empty? lis)
      [acc context]
      (let [this (first lis)
            remainder (next lis)
            [result new-context] (apply func [this context])]
        (recur new-context remainder (conj acc result))))))

(defn- consume [func initial-context]
  (loop [context initial-context
         acc []]
    (let [[result new-context] (apply func [context])]
      (if (not result)
        [acc new-context])
      (recur new-context (conj acc result)))))

(defn- consume-while [func initial-context]
  (loop [context initial-context
         acc []]
    (let [[result continue new-context] (apply func [context])]
      (if (not continue)
        [acc context]
        (recur new-context (conj acc result))))))

(defn- unzip-map [m]
  "Take a  map that has pairs in the value slots and produce a pair of maps,
   the first having all the first elements of the pairs and the second all
   the second elements of the pairs"
  [(into {} (for [[k [v1 v2]] m] [k v1]))
   (into {} (for [[k [v1 v2]] m] [k v2]))])

(defn- tuple-map [m v1]
  "For all the values, v, in the map, replace them with [v v1]"
  (into {} (for [[k v] m] [k [v v1]])))

(defn- rtrim [s c]
  "Trim all instances of c from the end of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s (dec (count s))) c))
      (loop [n (dec len)]
        (cond
          (neg? n) ""
          (not (= (nth s n) c)) (subs s 0 (inc n))
          true (recur (dec n))))
      s)))

(defn- ltrim [s c]
  "Trim all instances of c from the beginning of sequence s"
  (let [len (count s)]
    (if (and (pos? len) (= (nth s 0) c))
      (loop [n 0]
        (if (or (= n len) (not (= (nth s n) c)))
          (subs s n)
          (recur (inc n))))
      s)))

(defn- prefix-count [aseq val]
  "Return the number of times that val occurs at the start of sequence aseq,
if val is a seq itself, count the number of times any element of val occurs at the
beginning of aseq"
  (let [test (if (coll? val) (set val) #{val})]
    (loop [pos 0]
      (if (or (= pos (count aseq)) (not (test (nth aseq pos))))
        pos
        (recur (inc pos))))))

(defn- prerr [& args]
  "Println to *err*"
  ;;No *err*
  (apply println* args))

;; Flush the pretty-print buffer without flushing the underlying stream
(defprotocol IPrettyFlush
  (-ppflush [this]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pprint/column_writer.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic ^{:private true} *default-page-width* 72)

(defn- get-field [this sym]
  (sym @@this))

(defn- set-field [this sym new-val]
  (swap! @this assoc sym new-val))

(defn- get-column [this]
  (get-field this :cur))

(defn- get-line [this]
  (get-field this :line))

(defn- get-max-column [this]
  (get-field this :max))

(defn- set-max-column [this new-max]
  (set-field this :max new-max)
  nil)

(defn- get-writer [this]
  (get-field this :base))

(defn- c-write-char [this c]
  (if (= c (int \newline))
    (do
      (set-field this :cur 0)
      (set-field this :line (inc (get-field this :line))))
    (set-field this :cur (inc (get-field this :cur))))
  (-write (get-field this :base) c))

(defn- column-writer
  ([writer] (column-writer writer *default-page-width*))
  ([writer max-columns]
    (let [fields (atom {:max max-columns, :cur 0, :line 0, :base writer})]
      (reify
        IDeref
        (-deref [_] fields)
        IWriter
        (-flush [_] (-flush writer))
        (-write
          ;;IWriter doesn't provide multi-arity, not sure of importance
          #_([cbuf off len]
            (let [writer (get-field this :base)]
              (-write writer cbuf off len)))
          [this x]
          (condp = (type x)
            js/String
            (let [s x
                  nl (.lastIndexOf s \newline)]
              (if (neg? nl)
                (set-field this :cur (+ (get-field this :cur) (count s)))
                (do
                  (set-field this :cur (- (count s) nl 1))
                  (set-field this :line (+ (get-field this :line)
                                          (count (filter #(= % \newline) s))))))
              (-write (get-field this :base) s))
            js/Number
            (c-write-char this x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pretty_writer.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forward declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare get-miser-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The data structures used by pretty-writer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ^{:private true} logical-block [parent section start-col indent
                                           done-nl intra-block-nl
                                           prefix per-line-prefix suffix
                                           logical-block-callback])

(defn- ancestor? [parent child]
  (loop [child (:parent child)]
    (cond
      (nil? child) false
      (identical? parent child) true
      :else (recur (:parent child)))))

(defn- buffer-length [l]
  (let [l (seq l)]
    (if l
      (- (:end-pos (last l)) (:start-pos (first l)))
      0)))

;; A blob of characters (aka a string)
(deftype buffer-blob :data :trailing-white-space :start-pos :end-pos :a)

;; A newline
(deftype nl-t :type :logical-block :start-pos :end-pos :a)

(deftype start-block-t :logical-block :start-pos :end-pos :a :b)

(deftype end-block-t :logical-block :start-pos :end-pos :a :b)

(deftype indent-t :logical-block :relative-to :offset :start-pos :end-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to write tokens in the output buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private pp-newline (fn [] "\n"))

(declare emit-nl)

(defmulti ^{:private true} write-token #(:type-tag %2))

(defmethod write-token :start-block-t [this token]
  (when-let [cb (getf :logical-block-callback)] (cb :start))
  (let [lb (:logical-block token)]
    (when-let [prefix (:prefix lb)]
      (-write (getf :base) prefix))
    (let [col (get-column (getf :base))]
      (reset! (:start-col lb) col)
      (reset! (:indent lb) col))))

(defmethod write-token :end-block-t [this token]
  (when-let [cb (getf :logical-block-callback)] (cb :end))
  (when-let [suffix (:suffix (:logical-block token))]
    (-write (getf :base) suffix)))

(defmethod write-token :indent-t [this token]
  (let [lb (:logical-block token)]
    (reset! (:indent lb)
      (+ (:offset token)
        (condp = (:relative-to token)
          :block @(:start-col lb)
          :current (get-column (getf :base)))))))

(defmethod write-token :buffer-blob [this token]
  (-write (getf :base) (:data token)))

(defmethod write-token :nl-t [this token]
  (if (or (= (:type token) :mandatory)
        (and (not (= (:type token) :fill))
          @(:done-nl (:logical-block token))))
    (emit-nl this token)
    (if-let [tws (getf :trailing-white-space)]
      (-write (getf :base) tws)))
  (setf :trailing-white-space nil))

(defn- write-tokens [this tokens force-trailing-whitespace]
  (doseq [token tokens]
    (if-not (= (:type-tag token) :nl-t)
      (if-let [tws (getf :trailing-white-space)]
        (-write (getf :base) tws)))
    (write-token this token)
    (setf :trailing-white-space (:trailing-white-space token))
    (let [tws (getf :trailing-white-space)]
      (when (and force-trailing-whitespace tws)
        (-write (getf :base) tws)
        (setf :trailing-white-space nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emit-nl? method defs for each type of new line. This makes
;; the decision about whether to print this type of new line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- tokens-fit? [this tokens]
  (let [maxcol (get-max-column (getf :base))]
    (or
      (nil? maxcol)
      (< (+ (get-column (getf :base)) (buffer-length tokens)) maxcol))))

(defn- linear-nl? [this lb section]
  (or @(:done-nl lb)
      (not (tokens-fit? this section))))

(defn- miser-nl? [this lb section]
  (let [miser-width (get-miser-width this)
        maxcol (get-max-column (getf :base))]
    (and miser-width maxcol
         (>= @(:start-col lb) (- maxcol miser-width))
         (linear-nl? this lb section))))

(defmulti ^{:private true} emit-nl? (fn [t _ _ _] (:type t)))

(defmethod emit-nl? :linear [newl this section _]
  (let [lb (:logical-block newl)]
    (linear-nl? this lb section)))

(defmethod emit-nl? :miser [newl this section _]
  (let [lb (:logical-block newl)]
    (miser-nl? this lb section)))

(defmethod emit-nl? :fill [newl this section subsection]
  (let [lb (:logical-block newl)]
    (or @(:intra-block-nl lb)
        (not (tokens-fit? this subsection))
        (miser-nl? this lb section))))

(defmethod emit-nl? :mandatory [_ _ _ _]
  true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various support functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-section [buffer]
  (let [nl (first buffer)
        lb (:logical-block nl)
        section (seq (take-while #(not (and (nl-t? %) (ancestor? (:logical-block %) lb)))
                       (next buffer)))]
    [section (seq (drop (inc (count section)) buffer))]))

(defn- get-sub-section [buffer]
  (let [nl (first buffer)
        lb (:logical-block nl)
        section (seq (take-while #(let [nl-lb (:logical-block %)]
                                   (not (and (nl-t? %) (or (= nl-lb lb) (ancestor? nl-lb lb)))))
                       (next buffer)))]
    section))

(defn- update-nl-state [lb]
  (reset! (:intra-block-nl lb) true)
  (reset! (:done-nl lb) true)
  (loop [lb (:parent lb)]
    (if lb
      (do (reset! (:done-nl lb) true)
          (reset! (:intra-block-nl lb) true)
          (recur (:parent lb))))))

(defn- emit-nl [this nl]
  (-write (getf :base) (pp-newline))
  (setf :trailing-white-space nil)
  (let [lb (:logical-block nl)
        prefix (:per-line-prefix lb)]
    (if prefix
      (-write (getf :base) prefix))
    (let [istr (apply str (repeat (- @(:indent lb) (count prefix)) \space))]
      (-write (getf :base) istr))
    (update-nl-state lb)))

(defn- split-at-newline [tokens]
  (let [pre (seq (take-while #(not (nl-t? %)) tokens))]
    [pre (seq (drop (count pre) tokens))]))

;; write-token-string is called when the set of tokens in the buffer
;; is long than the available space on the line

(defn- write-token-string [this tokens]
  (let [[a b] (split-at-newline tokens)]
    (if a (write-tokens this a false))
    (if b
      (let [[section remainder] (get-section b)
            newl (first b)]
        (let [do-nl (emit-nl? newl this section (get-sub-section b))
              result (if do-nl
                       (do
                         (emit-nl this newl)
                         (next b))
                       b)
              long-section (not (tokens-fit? this result))
              result (if long-section
                       (let [rem2 (write-token-string this section)]
                         (if (= rem2 section)
                           (do ; If that didn't produce any output, it has no nls
                             ; so we'll force it
                             (write-tokens this section false)
                             remainder)
                           (into [] (concat rem2 remainder))))
                       result)]
          result)))))

(defn- write-line [this]
  (loop [buffer (getf :buffer)]
    (setf :buffer (into [] buffer))
    (if (not (tokens-fit? this buffer))
      (let [new-buffer (write-token-string this buffer)]
        (if-not (identical? buffer new-buffer)
          (recur new-buffer))))))

;; Add a buffer token to the buffer and see if it's time to start
;; writing
(defn- add-to-buffer [this token]
  (setf :buffer (conj (getf :buffer) token))
  (if (not (tokens-fit? this (getf :buffer)))
    (write-line this)))

;; Write all the tokens that have been buffered
(defn- write-buffered-output [this]
  (write-line this)
  (if-let [buf (getf :buffer)]
    (do
      (write-tokens this buf true)
      (setf :buffer []))))

(defn- write-white-space [this]
  (when-let [tws (getf :trailing-white-space)]
    (-write (getf :base) tws)
    (setf :trailing-white-space nil)))

;; If there are newlines in the string, print the lines up until the last newline,
;; making the appropriate adjustments. Return the remainder of the string
(defn- write-initial-lines
  [this s]
  (let [lines (string/split s #"\n" -1)]
    (if (= (count lines) 1)
      s
      (let [prefix (:per-line-prefix (first (getf :logical-blocks)))
            l (first lines)]
        (if (= :buffering (getf :mode))
          (let [oldpos (getf :pos)
                newpos (+ oldpos (count l))]
            (setf :pos newpos)
            (add-to-buffer this (make-buffer-blob l nil oldpos newpos))
            (write-buffered-output this))
          (do
            (write-white-space this)
            (-write (getf :base) l)))
        (-write (getf :base) \newline)
        (doseq [l (next (butlast lines))]
          (-write (getf :base) l)
          (-write (getf :base) (pp-newline))
          (if prefix
            (-write (getf :base) prefix)))
        (setf :buffering :writing)
        (last lines)))))

(defn- p-write-char [this c]
  (if (= (getf :mode) :writing)
    (do
      (write-white-space this)
      (-write (getf :base) c))
    (if (= c \newline)
      (write-initial-lines this \newline)
      (let [oldpos (getf :pos)
            newpos (inc oldpos)]
        (setf :pos newpos)
        (add-to-buffer this (make-buffer-blob (str (char c)) nil oldpos newpos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize the pretty-writer instance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- pretty-writer [writer max-columns miser-width]
  (let [lb (logical-block. nil nil (atom 0) (atom 0) (atom false) (atom false)
                           nil nil nil nil)
        fields (atom {:pretty-writer true
                      :base (column-writer writer max-columns)
                      :logical-blocks lb
                      :sections nil
                      :mode :writing
                      :buffer []
                      :buffer-block lb
                      :buffer-level 1
                      :miser-width miser-width
                      :trailing-white-space nil
                      :pos 0})]
    (reify
      IDeref
      (-deref [_] fields)

      IWriter
      (-write [this x]
        (condp = (type x)
          js/String
          (let [s0 (write-initial-lines this x)
                s (string/replace-first s0 #"\s+$" "")
                white-space (subs s0 (count s))
                mode (getf :mode)]
            (if (= mode :writing)
              (do
                (write-white-space this)
                (-write (getf :base) s)
                (setf :trailing-white-space white-space))
              (let [oldpos (getf :pos)
                    newpos (+ oldpos (count s0))]
                (setf :pos newpos)
                (add-to-buffer this (make-buffer-blob s white-space oldpos newpos)))))
          js/Number
          (p-write-char this x)))
      (-flush [this]
        (-ppflush this)
        (-flush (getf :base)))
      ;;no -close

      IPrettyFlush
      (-ppflush [this]
        (if (= (getf :mode) :buffering)
          (do
            (write-tokens this (getf :buffer) true)
            (setf :buffer []))
          (write-white-space this))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods for pretty-writer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- start-block
  [this prefix per-line-prefix suffix]
  (let [lb (logical-block. (getf :logical-blocks) nil (atom 0) (atom 0)
                           (atom false) (atom false)
                           prefix per-line-prefix suffix nil)]
    (setf :logical-blocks lb)
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (when-let [cb (getf :logical-block-callback)] (cb :start))
        (if prefix
          (-write (getf :base) prefix))
        (let [col (get-column (getf :base))]
          (reset! (:start-col lb) col)
          (reset! (:indent lb) col)))
      (let [oldpos (getf :pos)
            newpos (+ oldpos (if prefix (count prefix) 0))]
        (setf :pos newpos)
        (add-to-buffer this (make-start-block-t lb oldpos newpos))))))

(defn- end-block [this]
  (let [lb (getf :logical-blocks)
        suffix (:suffix lb)]
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (if suffix
          (-write (getf :base) suffix))
        (when-let [cb (getf :logical-block-callback)] (cb :end)))
      (let [oldpos (getf :pos)
            newpos (+ oldpos (if suffix (count suffix) 0))]
        (setf :pos newpos)
        (add-to-buffer this (make-end-block-t lb oldpos newpos))))
    (setf :logical-blocks (:parent lb))))

(defn- nl [this type]
  (setf :mode :buffering)
  (let [pos (getf :pos)]
    (add-to-buffer this (make-nl-t type (getf :logical-blocks) pos pos))))

(defn- indent [this relative-to offset]
  (let [lb (getf :logical-blocks)]
    (if (= (getf :mode) :writing)
      (do
        (write-white-space this)
        (reset! (:indent lb)
                (+ offset (condp = relative-to
                            :block @(:start-col lb)
                            :current (get-column (getf :base))))))
      (let [pos (getf :pos)]
        (add-to-buffer this (make-indent-t lb relative-to offset pos pos))))))

(defn- get-miser-width [this]
  (getf :miser-width))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pprint_base.clj
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables that control the pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *print-length*, *print-level* and *print-dup* are defined in cljs.core

(def ^:dynamic
 ^{:doc "Bind to true if you want write to use pretty printing"}
 *print-pretty* true)

(defonce ^:dynamic
 ^{:doc "The pretty print dispatch function. Use with-pprint-dispatch or
 set-pprint-dispatch to modify."}
 *print-pprint-dispatch* nil)

(def ^:dynamic
 ^{:doc "Pretty printing will try to avoid going beyond this column.
 Set to nil to have pprint let the line be arbitrarily long. This will ignore all
 non-mandatory newlines."}
 *print-right-margin* 72)

(def ^:dynamic
 ^{:doc "The column at which to enter miser style. Depending on the dispatch table,
 miser style add newlines in more places to try to keep lines short allowing for further
 levels of nesting."}
 *print-miser-width* 40)

;;; TODO implement output limiting
(def ^:dynamic
 ^{:private true,
  :doc "Maximum number of lines to print in a pretty print instance (N.B. This is not yet used)"}
 *print-lines* nil)

;;; TODO: implement circle and shared
(def ^:dynamic
 ^{:private true,
  :doc "Mark circular structures (N.B. This is not yet used)"}
 *print-circle* nil)

;;; TODO: should we just use *print-dup* here?
(def ^:dynamic
 ^{:private true,
  :doc "Mark repeated structures rather than repeat them (N.B. This is not yet used)"}
*print-shared* nil)

(def ^:dynamic
 ^{:doc "Don't print namespaces with symbols. This is particularly useful when
pretty printing the results of macro expansions"}
 *print-suppress-namespaces* nil)

;;; TODO: support print-base and print-radix in cl-format
;;; TODO: support print-base and print-radix in rationals
(def ^:dynamic
 ^{:doc "Print a radix specifier in front of integers and rationals. If *print-base* is 2, 8,
or 16, then the radix specifier used is #b, #o, or #x, respectively. Otherwise the
radix specifier is in the form #XXr where XX is the decimal value of *print-base* "}
 *print-radix* nil)

(def ^:dynamic
^{:doc "The base to use for printing integers and rationals."}
*print-base* 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal variables that keep track of where we are in the
;; structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic ^{:private true} *current-level* 0)

(def ^:dynamic ^{:private true} *current-length* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the write function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare format-simple-number)

(def ^{:private true} orig-pr* pr*)

(defn- pr-with-base [x]
  (if-let [s (format-simple-number x)]
    (print* s)
    (orig-pr* x)))

(def ^{:private true} write-option-table
  {;:array            *print-array*
   :base             #'cljs.contrib.pprint/*print-base*,
   ;;:case             *print-case*,
   :circle           #'cljs.contrib.pprint/*print-circle*,
   ;;:escape           *print-escape*,
   ;;:gensym           *print-gensym*,
   :length           #'cljs.core/*print-length*,
   :level            #'cljs.core/*print-level*,
   :lines            #'cljs.contrib.pprint/*print-lines*,
   :miser-width      #'cljs.contrib.pprint/*print-miser-width*,
   :dispatch         #'cljs.contrib.pprint/*print-pprint-dispatch*,
   :pretty           #'cljs.contrib.pprint/*print-pretty*,
   :radix            #'cljs.contrib.pprint/*print-radix*,
   :readably         #'cljs.core/*print-readably*,
   :right-margin     #'cljs.contrib.pprint/*print-right-margin*,
   :suppress-namespaces #'cljs.contrib.pprint/*print-suppress-namespaces*})

(defn- table-ize [t m]
  (apply hash-map (mapcat
                    #(when-let [v (get t (key %))] [v (val %)])
                    m)))

(defn- pretty-writer?
  "Return true iff x is a PrettyWriter"
  [x] (and (satisfies? IDeref x) (:pretty-writer @@x)))

(defn- make-pretty-writer
  "Wrap base-writer in a PrettyWriter with the specified right-margin and miser-width"
  [base-writer right-margin miser-width]
  (pretty-writer base-writer right-margin miser-width))

;;;TODO: if pretty print is not set, don't use pr but rather something that respects *print-base*, etc.
(defn write-out
  "Write an object to *out* subject to the current bindings of the printer control
variables. Use the kw-args argument to override individual variables for this call (and
any recursive calls).

*out* must be a PrettyWriter if pretty printing is enabled. This is the responsibility
of the caller.

This method is primarily intended for use by pretty print dispatch functions that
already know that the pretty printer will have set up their environment appropriately.
Normal library clients should use the standard \"write\" interface. "
  [object]
  (let [length-reached (and *current-length*
                            *print-length*
                            (>= *current-length* *print-length*))]
    (if-not *print-pretty*
      (pr* object)                                           ;;TODO need to pr to *out*
      (if length-reached
        (-write *out* "...")
        (do
          (if *current-length* (set! *current-length* (inc *current-length*)))
          (*print-pprint-dispatch* object))))
    length-reached))

(defn write
  "Write an object subject to the current bindings of the printer control variables.
Use the kw-args argument to override individual variables for this call (and any
recursive calls). Returns the string result if :stream is nil or nil otherwise.

The following keyword arguments can be passed with values:
  Keyword              Meaning                              Default value
  :stream              Writer for output or nil             true (indicates *out*)
  :base                Base to use for writing rationals    Current value of *print-base*
  :circle*             If true, mark circular structures    Current value of *print-circle*
  :length              Maximum elements to show in sublists Current value of *print-length*
  :level               Maximum depth                        Current value of *print-level*
  :lines*              Maximum lines of output              Current value of *print-lines*
  :miser-width         Width to enter miser mode            Current value of *print-miser-width*
  :dispatch            The pretty print dispatch function   Current value of *print-pprint-dispatch*
  :pretty              If true, do pretty printing          Current value of *print-pretty*
  :radix               If true, prepend a radix specifier   Current value of *print-radix*
  :readably*           If true, print readably              Current value of *print-readably*
  :right-margin        The column for the right margin      Current value of *print-right-margin*
  :suppress-namespaces If true, no namespaces in symbols    Current value of *print-suppress-namespaces*

  * = not yet supported
"
  {:added "1.2"}
  [object & kw-args]
  (let [options (merge {:stream true} (apply hash-map kw-args))]
    #_(binding-map (table-ize write-option-table options)
                 #_(binding-map (if (or (not (= *print-base* 10)) *print-radix*) {#'pr* pr-with-base} {})
                              (let [sb (StringBuffer.)
                                    optval (if (contains? options :stream)
                                             (:stream options)
                                             true)
                                    base-writer (condp = optval
                                                  nil (StringBufferWriter. sb)
                                                  true *out*
                                                  optval)]
                                (if *print-pretty*
                                  (with-pretty-writer base-writer
                                    (write-out object))
                                  (binding [*out* base-writer]
                                    (pr* object)))
                                (if (nil? optval)
                                  (str sb)))))))

(defn pprint
  ([object]
   (let [sb (StringBuffer.)]
     (binding [*out* (StringBufferWriter. sb)]
       (pprint object *out*)
       (*print-fn* (str sb)))))
  ([object writer]
   (with-pretty-writer writer
     (binding [*print-pretty* true]
       ;;We can't conditionally bind, so we can't exactly duplicate the clj macro
       (if (or (not (= *print-base* 10)) *print-radix*)
         (binding [pr* pr-with-base]
           (write-out object))
         (write-out object)))
     (if (not (= 0 (get-column *out*)))
       (-write *out* \newline)))
    nil))

(defn set-pprint-dispatch
  [function]
  (set! *print-pprint-dispatch* function)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the functional interface to the pretty printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- check-enumerated-arg [arg choices]
  (if-not (choices arg)
    ;; TODO clean up choices string
    (throw (js/Error. (str "Bad argument: " arg ". It must be one of " choices)))))

(defn- level-exceeded []
  (and *print-level* (>= *current-level* *print-level*)))

(defn pprint-newline
  "Print a conditional newline to a pretty printing stream. kind specifies if the
  newline is :linear, :miser, :fill, or :mandatory.

  This function is intended for use when writing custom dispatch functions.

  Output is sent to *out* which must be a pretty printing writer."
  [kind]
  (check-enumerated-arg kind #{:linear :miser :fill :mandatory})
  (nl *out* kind))

(defn pprint-indent
  "Create an indent at this point in the pretty printing stream. This defines how
following lines are indented. relative-to can be either :block or :current depending
whether the indent should be computed relative to the start of the logical block or
the current column position. n is an offset.

This function is intended for use when writing custom dispatch functions.

Output is sent to *out* which must be a pretty printing writer."
  [relative-to n]
  (check-enumerated-arg relative-to #{:block :current})
  (indent *out* relative-to n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pprint/cl_format.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Forward references
(declare compile-format)
(declare execute-format)
(declare init-navigator)
;; End forward references

(defn cl-format
  "An implementation of a Common Lisp compatible format function. cl-format formats its
arguments to an output stream or string based on the format control string given. It
supports sophisticated formatting of structured data.

Writer satisfies IWriter, true to output via *print-fn* or nil to output
to a string, format-in is the format control string and the remaining arguments
are the data to be formatted.

The format control string is a string to be output with embedded 'format directives'
describing how to format the various arguments passed in.

If writer is nil, cl-format returns the formatted result string. Otherwise, cl-format
returns nil.

For example:
 (let [results [46 38 22]]
        (cl-format true \"There ~[are~;is~:;are~]~:* ~d result~:p: ~{~d~^, ~}~%\"
                   (count results) results))

Prints via *print-fn*:
 There are 3 results: 46, 38, 22

Detailed documentation on format control strings is available in the \"Common Lisp the
Language, 2nd edition\", Chapter 22 (available online at:
http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000)
and in the Common Lisp HyperSpec at
http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm"
  {:see-also [["http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000"
               "Common Lisp the Language"]
              ["http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm"
               "Common Lisp HyperSpec"]]}
  [writer format-in & args]
  (let [compiled-format (if (string? format-in) (compile-format format-in) format-in)
        navigator (init-navigator args)]
    (execute-format writer compiled-format navigator)))

(def ^:dynamic ^{:private true} *format-str* nil)

(defn- format-error [message offset]
  (let [full-message (str message \newline *format-str* \newline
                          (apply str (repeat offset \space)) "^" \newline)]
    (throw (js/Error full-message))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument navigators manage the argument list
;; as the format statement moves through the list
;; (possibly going forwards and backwards as it does so)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord ^{:private true}
  arg-navigator [seq rest pos])

(defn- init-navigator
  "Create a new arg-navigator from the sequence with the position set to 0"
  {:skip-wiki true}
  [s]
  (let [s (seq s)]
    (arg-navigator. s s 0)))

;; TODO call format-error with offset
(defn- next-arg [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (arg-navigator. (:seq navigator) (next rst) (inc (:pos navigator)))]
      (throw (js/Error "Not enough arguments for format definition")))))

(defn- next-arg-or-nil [navigator]
  (let [rst (:rest navigator)]
    (if rst
      [(first rst) (arg-navigator. (:seq navigator) (next rst) (inc (:pos navigator)))]
      [nil navigator])))

;; Get an argument off the arg list and compile it if it's not already compiled
(defn- get-format-arg [navigator]
  (let [[raw-format navigator] (next-arg navigator)
        compiled-format (if (string? raw-format)
                          (compile-format raw-format)
                          raw-format)]
    [compiled-format navigator]))

(declare relative-reposition)

(defn- absolute-reposition [navigator position]
  (if (>= position (:pos navigator))
    (relative-reposition navigator (- (:pos navigator) position))
    (arg-navigator. (:seq navigator) (drop position (:seq navigator)) position)))

(defn- relative-reposition [navigator position]
  (let [newpos (+ (:pos navigator) position)]
    (if (neg? position)
      (absolute-reposition navigator newpos)
      (arg-navigator. (:seq navigator) (drop position (:rest navigator)) newpos))))

(defrecord ^{:private true}
  compiled-directive [func def params offset])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; When looking at the parameter list, we may need to manipulate
;; the argument list as well (for 'V' and '#' parameter types).
;; We hide all of this behind a function, but clients need to
;; manage changing arg navigator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: validate parameters when they come from arg list
(defn- realize-parameter [[param [raw-val offset]] navigator]
  (let [[real-param new-navigator]
        (cond
          (contains? #{:at :colon} param) ;pass flags through unchanged - this really isn't necessary
          [raw-val navigator]

          (= raw-val :parameter-from-args)
          (next-arg navigator)

          (= raw-val :remaining-arg-count)
          [(count (:rest navigator)) navigator]

          true
          [raw-val navigator])]
    [[param [real-param offset]] new-navigator]))

(defn- realize-parameter-list [parameter-map navigator]
  (let [[pairs new-navigator]
        (map-passing-context realize-parameter navigator parameter-map)]
    [(into {} pairs) new-navigator]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that support individual directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common handling code for ~A and ~S
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare opt-base-str)

(def ^{:private true}
     special-radix-markers {2 "#b" 8 "#o" 16 "#x"})

(defn- format-simple-number [n]
  (cond
    (integer? n) (if (= *print-base* 10)
                   (str n (if *print-radix* "."))
                   (str
                     (if *print-radix* (or (get special-radix-markers *print-base*) (str "#" *print-base* "r")))
                     (opt-base-str *print-base* n)))
    #_(ratio? n) ;;no ratio support
    :else nil))

(defn- format-ascii [print-func params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator)
        base-output (or (format-simple-number arg) (print-func arg))
        base-width (.-length base-output)
        min-width (+ base-width (:minpad params))
        width (if (>= min-width (:mincol params))
                min-width
                (+ min-width
                   (* (+ (quot (- (:mincol params) min-width 1)
                               (:colinc params))
                         1)
                      (:colinc params))))
        chars (apply str (repeat (- width base-width) (:padchar params)))]
    (if (:at params)
      (print* (str chars base-output))
      (print* (str base-output chars)))
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the integer directives ~D, ~X, ~O, ~B and some
;; of ~R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- integral?
  "returns true if a number is actually an integer (that is, has no fractional part)"
  [x]
  (cond
    (integer? x) true
    (float? x) (= x (Math/floor x))
    :else false))

(defn- remainders
  "Return the list of remainders (essentially the 'digits') of val in the given base"
  [base val]
  (reverse
    (first
      (consume #(if (pos? %)
                 [(rem % base) (quote %) base]
                 [nil nil])
               val))))

;; TODO: xlated-val does not seem to be used here.
;; NB
(defn- base-str
  "Return val as a string in the given base"
  [base val]
  (if (zero? val)
    "0"
    (let [xlated-val (cond
                       ;(float? val) (bigdec val) ;;No bigdec
                       ;(ratio? val) nil ;;No ratio
                       :else val)]
      (apply str
             (map
               #(if (< % 10) (char (+ (int \0) %)) (char (+ (int \a) (- % 10))))
               (remainders base val))))))

;;Not sure if this is accurate or necessary
(def ^{:private true}
     javascript-base-formats {8 "%o", 10 "%d", 16 "%x"})

(defn- opt-base-str
  "Return val as a string in the given base. No cljs format, so no improved performance."
  [base val]
  (base-str base val))

(defn- group-by* [unit lis]
  (reverse
    (first
      (consume (fn [x] [(seq (reverse (take unit x))) (seq (drop unit x))]) (reverse lis)))))

(defn- format-integer [base params arg-navigator offsets]
  (let [[arg arg-navigator] (next-arg arg-navigator)]
    (if (integral? arg)
      (let [neg (neg? arg)
            pos-arg (if neg (- arg) arg)
            raw-str (opt-base-str base pos-arg)
            group-str (if (:colon params)
                        (let [groups (map #(apply str %) (group-by* (:commainterval params) raw-str))
                              commas (repeat (count groups) (:commachar params))]
                          (apply str (next (interleave commas groups))))
                        raw-str)
            signed-str (cond
                         neg (str "-" group-str)
                         (:at params) (str "+" group-str)
                         true group-str)
            padded-str (if (< (.-length signed-str) (:mincol params))
                         (str (apply str (repeat (- (:mincol params) (.-length signed-str))
                                                 (:padchar params)))
                              signed-str)
                         signed-str)]
        (print padded-str))
      (format-ascii print-str {:mincol (:mincol params) :colinc 1 :minpad 0
                               :padchar (:padchar params) :at true}
                    (init-navigator [arg]) nil))
    arg-navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for english formats (~R and ~:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:private true}
     english-cardinal-units
     ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
      "ten" "eleven" "twelve" "thirteen" "fourteen"
      "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])

(def ^{:private true}
     english-ordinal-units
     ["zeroth" "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"
      "tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
      "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"])

(def ^{:private true}
     english-cardinal-tens
     ["" "" "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])

(def ^{:private true}
     english-ordinal-tens
     ["" "" "twentieth" "thirtieth" "fortieth" "fiftieth"
      "sixtieth" "seventieth" "eightieth" "ninetieth"])

;; We use "short scale" for our units (see http://en.wikipedia.org/wiki/Long_and_short_scales)
;; Number names from http://www.jimloy.com/math/billion.htm
;; We follow the rules for writing numbers from the Blue Book
;; (http://www.grammarbook.com/numbers/numbers.asp)
(def ^{:private true}
     english-scale-numbers
     ["" "thousand" "million" "billion" "trillion" "quadrillion" "quintillion"
      "sextillion" "septillion" "octillion" "nonillion" "decillion"
      "undecillion" "duodecillion" "tredecillion" "quattuordecillion"
      "quindecillion" "sexdecillion" "septendecillion"
      "octodecillion" "novemdecillion" "vigintillion"])

(defn- format-simple-cardinal
  "Convert a number less than 1000 to a cardinal english string"
  [num]
  (let [hundreds (quot num 100)
        tens (rem num 100)]
    (str
      (if (pos? hundreds) (str (nth english-cardinal-units hundreds) " hundred"))
      (if (and (pos? hundreds) (pos? tens)) " ")
      (if (pos? tens)
        (if (< tens 20)
          (nth english-cardinal-units tens)
          (let [ten-digit (quot tens 10)
                unit-digit (rem tens 10)]
            (str
              (if (pos? ten-digit) (nth english-cardinal-tens ten-digit))
              (if (and (pos? ten-digit) (pos? unit-digit)) "-")
              (if (pos? unit-digit) (nth english-cardinal-units unit-digit)))))))))

(defn- add-english-scales
  "Take a sequence of parts, add scale numbers (e.g., million) and combine into a string
  offset is a factor of 10^3 to multiply by"
  [parts offset]
  (let [cnt (count parts)]
    (loop [acc []
           pos (dec cnt)
           this (first parts)
           remainder (next parts)]
      (if (nil? remainder)
        (str (apply str (interpose ", " acc))
             (if (and (not (empty? this)) (not (empty? acc))) ", ")
             this
             (if (and (not (empty? this)) (pos? (+ pos offset)))
               (str " " (nth english-scale-numbers (+ pos offset)))))
        (recur
          (if (empty? this)
            acc
            (conj acc (str this " " (nth english-scale-numbers (+ pos offset)))))
          (dec pos)
          (first remainder)
          (next remainder))))))

(defn- format-cardinal-english [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (= 0 arg)
      (print* "zero")
      (let [abs-arg (if (neg? arg) (- arg) arg) ; some numbers are too big for Math/abs (is this true?)
            parts (remainders 1000 abs-arg)]
        (if (<= (count parts) (count english-scale-numbers))
          (let [parts-strs (map format-simple-cardinal parts)
                full-str (add-english-scales parts-strs 0)]
            (print* (str (if (neg? arg) "minus ") full-str)))
          (format-integer ;; for numbers > `0^63, we fall back on ~D
            10
            {:mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
            (init-navigator [arg])
            {:mincol 0, :padchar 0, :commachar 0 :commainterval 0}))))
    navigator))

(defn- format-simple-ordinal
  "Convert a number less than 1000 to a ordinal english string
  Note this should only be used for the last one in the sequence"
  [num]
  (let [hundreds (quot num 100)
        tens (rem num 100)]
    (str
      (if (pos? hundreds) (str (nth english-cardinal-units hundreds) " hundred"))
      (if (and (pos? hundreds) (pos? tens)) " ")
      (if (pos? tens)
        (if (< tens 20)
          (nth english-ordinal-units tens)
          (let [ten-digit (quot tens 10)
                unit-digit (rem tens 10)]
            (if (and (pos? ten-digit) (not (pos? unit-digit)))
              (nth english-ordinal-units ten-digit)
              (str
                (if (pos? ten-digit) (nth english-cardinal-tens ten-digit))
                (if (and (pos? ten-digit) (pos? unit-digit)) "-")
                (if (pos? unit-digit) (nth english-ordinal-units unit-digit))))))
        (if (pos? hundreds) "th")))))

(defn- format-ordinal-english [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (= 0 arg)
      (print* "zeroth")
      (let [abs-arg (if (neg? arg) (- arg) arg) ; some numbers are too big for Math/abs (is this true?)
            parts (remainders 1000 abs-arg)]
        (if (<= (count parts) (count english-scale-numbers))
          (let [parts-strs (map format-simple-cardinal (drop-last parts))
                head-str (add-english-scales parts-strs 1)
                tail-str (format-simple-ordinal (last parts))]
            (print* (str (if (neg? arg) "minus ")
                         (cond
                           (and (not (empty? head-str)) (not (empty? tail-str)))
                           (str head-str ", " tail-str)

                           (not (empty? head-str)) (str head-str "th")
                           :else tail-str))))
          (do (format-integer ;for numbers > 10^63, we fall back on ~D
                10
                {:mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
                (init-navigator [arg])
                {:mincol 0, :padchar 0, :commachar 0 :commainterval 0})
              (let [low-two-digits (rem arg 100)
                    not-teens (or (< 11 low-two-digits) (> 19 low-two-digits))
                    low-digit (rem low-two-digits 10)]
                (print* (cond
                          (and (== low-digit 1) not-teens) "st"
                          (and (== low-digit 2) not-teens) "nd"
                          (and (== low-digit 3) not-teens) "rd"
                          :else "th")))))))
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for roman numeral formats (~@R and ~@:R)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:private true}
     old-roman-table
     [[ "I" "II" "III" "IIII" "V" "VI" "VII" "VIII" "VIIII"]
      [ "X" "XX" "XXX" "XXXX" "L" "LX" "LXX" "LXXX" "LXXXX"]
      [ "C" "CC" "CCC" "CCCC" "D" "DC" "DCC" "DCCC" "DCCCC"]
      [ "M" "MM" "MMM"]])

(def ^{:private true}
     new-roman-table
     [[ "I" "II" "III" "IV" "V" "VI" "VII" "VIII" "IX"]
      [ "X" "XX" "XXX" "XL" "L" "LX" "LXX" "LXXX" "XC"]
      [ "C" "CC" "CCC" "CD" "D" "DC" "DCC" "DCCC" "CM"]
      [ "M" "MM" "MMM"]])

(defn- format-roman
  "Format a roman numeral using the specified look-up table"
  [table params navigator offsets]
  (let [[arg navigator] (next-arg navigator)]
    (if (and (number? arg) (> arg 0) (< arg 4000))
      (let [digits (remainders 10 arg)]
        (loop [acc []
               pos (dec (count digits))
               digits digits]
          (if (empty? digits)
            (print* (apply str acc))
            (let [digit (first digits)]
              (recur (if (= 0 digit)
                       acc
                       (conj acc (nth (nth table pos) (dec digit))))
                     (dec pos)
                     (next digits))))))
      (format-integer ; for anything <= 0 or > 3999, we fall back on ~D
        10
        {:mincol 0, :padchar \space, :commachar \, :commainterval 3, :colon true}
        (init-navigator [arg])
        {:mincol 0, :padchar 0, :commachar 0 :commainterval 0}))
    navigator))

(defn- format-old-roman [params navigator offsets]
  (format-roman old-roman-table params navigator offsets))

(defn- format-new-roman [params navigator offsets]
  (format-roman new-roman-table params navigator offsets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for character formats (~C)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:private true}
     special-chars {8 "Backspace", 9 "Tab", 10 "Newline", 13 "Return", 32 "Space"})

(defn- pretty-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)
        as-int (int c)
        base-char (bit-and as-int 127)
        meta (bit-and as-int 128)
        special (get special-chars base-char)]
    (if (> meta 0) (print* "Meta-"))
    (print* (cond
              special special
              (< base-char 32) (str "Control-" (char (+ base-char 64)))
              (= base-char 127) "Control-?"
              :else (char base-char)))
    navigator))

(defn- readable-character [params navigator offsets]
  (let [[c navigator] (next-arg navigator)]
    (condp = (:char-format params)
      \o (cl-format true "\\o~3, '0o" (int c))
      \u (cl-format true "\\u~4, '0x" (int c))
      nil (pr* c))
    navigator))

(defn- plain-character [params navigator offsets]
  (let [[char navigator] (next-arg navigator)]
    (print* char)
    navigator))

;; Check to see if a result is an abort (~^) construct
;; TODO: move these funcs somewhere more appropriat
(defn- abort? [context]
  (let [token (first context)]
    (or (= :up-arrow token) (= :colon-up-arrow token))))

;; Handle the execution of "sub-clauses" in bracket constructions
(defn- execute-sub-format [format args base-args]
  (second
    (map-passing-context
      (fn [element context]
        (if (abort? context)
          [nil context]    ; just keep passing it along
          (let [[params args] (realize-parameter-list (:params element) context)
                [params offsets] (unzip-map params)
                params (assoc params :base-args base-args)]
            [nil (apply (:func element) [params args offsets])])))
      args
      format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for real number formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - return exponent as int to eliminate double conversion
(defn- float-parts-base
  "Produce string parts for the mantissa (normalize 1-9) and exponent"
  [f]
  (let [s (string/lower-case (str f))
        exploc (.indexOf s \e)
        dotloc (.indexOf s \.)]
    (if (neg? exploc)
      (if (neg? dotloc)
        [s (str (dec (count s)))]
        [(str (subs s 0 dotloc) (subs s (inc dotloc))) (str (dec dotloc))])
      (if (neg? dotloc)
        [(subs s 0 exploc) (subs s (inc exploc))]
        [(str (subs s 0 1) (subs s 2 exploc)) (subs s (inc exploc))]))))

(defn- float-parts
  "Take care of leading and trailing zeros in decomposed floats"
  [f]
  (let [[m e] (float-parts-base f)
        m1 (rtrim m \0)
        m2 (ltrim m1 \0)
        delta (- (count m1) (count m2))
        e (if (and (pos? (count e)) (= (nth e 0) \+)) (subs e 1) e)]
    (if (empty? m2)
      ["0" 0]
      [m2 (- (js/parseInt e) delta)])))

(defn- inc-s
  "Assumption: The input string consists of one or more decimal digits,
  and no other characters. Return a string containing one or more
  decimal digits containing a decimal number one larger than the input
  string. The output string will always be the same length as the input
  string, or one character longer."
  [s]
  (let [len-1 (dec (count s))]
    (loop [i (int len-1)]
      (cond
        (neg? i) (apply str "1" (repeat (inc len-1) "0"))
        (= \9 (.charAt s i)) (recur (dec i))
        :else (apply str (subs s 0 i)
                     (char (inc (int (.charAt s i))))
                     (repeat (- len-1 i) "0"))))))

(defn- round-str [m e d w]
  (if (or d w)
    (let [len (count m)
          ;; Every formatted floating point number should include at
          ;; least one decimal digit and a decimal point.
          w (if w (max 2 w))
          round-pos (cond
                      ;; If d was given, that forces the rounding
                      ;; position, regardless of any width that may
                      ;; have been specified.
                      d (+ e d 1)
                      ;; Otherwise w was specified, so pick round-pos
                      ;; based upon that.
                      ;; If e>=0, then abs value of number is >= 1.0,
                      ;; and e+1 is number of decimal digits before the
                      ;; decimal point when the number is written
                      ;; without scientific notation. Never round the
                      ;; number before the decimal point.
                      (>= e 0) (max (inc e) (dec w))
                      ;; e < 0, so number abs value < 1.0
                      :else (+ w e))
          [m1 e1 round-pos len] (if (= round-pos 0)
                                  [(str "0" m) (inc e) 1 (inc len)]
                                  [m e round-pos len])]
      (if round-pos
        (if (neg? round-pos)
          ["0" 0 false]
          (if (> len round-pos)
            (let [round-char (nth m1 round-pos)
                  result (subs m1 0 round-pos)]
              (if (>= (int round-char) (int \5))
                (let [round-up-result (inc-s result)
                      expanded (> (count round-up-result) (count result))]
                  [(if expanded
                     (subs round-up-result 0 (dec (count round-up-result)))
                     round-up-result)
                   e1 expanded])
                [result e1 false]))
            [m e false]))
        [m e false]))
    [m e false]))

(defn- expand-fixed [m e d]
  (let [[m1 e1] (if (neg? e)
                  [(str (apply str (repeat (dec (- e)) \0)) m) -1]
                  [m e])
        len (count m1)
        target-len (if d (+ e1 d 1) (inc e1))]
    (if (< len target-len)
      (str m1 (apply str (repeat (- target-len len) \0)))
      m1)))

(defn- insert-decimal
  "Insert the decimal point at the right spot in the number to match an exponent"
  [m e]
  (if (neg? e)
    (str "." m)
    (let [loc (inc e)]
      (str (subs m 0 loc) "." (subs m loc)))))

(defn- get-fixed [m e d]
  (insert-decimal (expand-fixed m e d) e))

(defn- insert-scaled-decimal
  "Insert the decimal point at the right spot in the number to match an exponent"
  [m k]
  (if (neg? k)
    (str "." m)
    (str (subs m 0 k) "." (subs m k))))

;;TODO: No ratio, so not sure what to do here
(defn- convert-ratio [x]
  x)

;; the function to render ~F directives
;; TODO: support rationals. Back off to ~D/~A in the appropriate cases
(defn- fixed-float [params navigator offsets]
  (let [w (:w params)
        d (:d params)
        [arg navigator] (next-arg navigator)
        [sign abs] (if (neg? arg) ["-" (- arg)] ["+" arg])
        abs (convert-ratio abs)
        [mantissa exp] (float-parts abs)
        scaled-exp (+ exp (:k params))
        add-sign (or (:at params) (neg? arg))
        append-zero (and (not d) (<= (dec (count mantissa)) scaled-exp))
        [rounded-mantissa scaled-exp expanded] (round-str mantissa scaled-exp
                                                          d (if w (- w (if add-sign 1 0))))
        fixed-repr (get-fixed rounded-mantissa (if expanded (inc scaled-exp) scaled-exp) d)
        fixed-repr (if (and w d
                            (>= d 1)
                            (= (.charAt fixed-repr 0) \0)
                            (= (.charAt fixed-repr 1) \.)
                            (> (count fixed-repr) (- w (if add-sign 1 0))))
                     (subs fixed-repr 1)    ;chop off leading 0
                     fixed-repr)
        prepend-zero (= (first fixed-repr) \.)]
    (if w
      (let [len (count fixed-repr)
            signed-len (if add-sign (inc len) len)
            prepend-zero (and prepend-zero (not (>= signed-len w)))
            append-zero (and append-zero (not (>= signed-len w)))
            full-len (if (or prepend-zero append-zero)
                       (inc signed-len)
                       signed-len)]
        (if (and (> full-len w) (:overflowchar params))
          (print* (apply str (repeat w (:overflowchar params))))
          (print* (str
                    (apply str (repeat (- w full-len) (:padchar params)))
                    (if add-sign sign)
                    (if prepend-zero "0")
                    fixed-repr
                    (if append-zero "0")))))
      (print* (str
                (if add-sign sign)
                (if prepend-zero "0")
                fixed-repr
                (if append-zero "0"))))
    navigator))

;; the function to render ~E directives
;; TODO: support rationals. Back off to ~D/~A in the appropriate cases
;; TODO: define ~E representation for Infinity
(defn- exponential-float [params navigator offset]
  (let [[arg navigator] (next-arg navigator)
        arg (convert-ratio arg)]
    (loop [[mantissa exp] (float-parts (if (neg? arg) (- arg) arg))]
      (let [w (:w params)
            d (:d params)
            e (:e params)
            k (:k params)
            expchar (or (:exponentchar params) \E)
            add-sign (or (:at params) (neg? arg))
            prepend-zero (<= k 0)
            scaled-exp (- exp (dec k))
            scaled-exp-str (str (Math/abs scaled-exp))
            scaled-exp-str (str expchar (if (neg? scaled-exp) \- \+)
                                (if e (apply str
                                             (repeat
                                               (- e
                                                  (count scaled-exp-str))
                                               \0)))
                                scaled-exp-str)
            exp-width (count scaled-exp-str)
            base-mantissa-width (count mantissa)
            scaled-mantissa (str (apply str (repeat (- k) \0))
                                 mantissa
                                 (if d
                                   (apply str
                                          (repeat
                                            (- d (dec base-mantissa-width)
                                               (if (neg? k) (- k) 0)) \0))))
            w-mantissa (if w (- w exp-width))
            [rounded-mantissa _ incr-exp] (round-str
                                            scaled-mantissa 0
                                            (cond
                                              (= k 0) (dec d)
                                              (pos? k) d
                                              (neg? k) (dec d))
                                            (if w-mantissa
                                              (- w-mantissa (if add-sign 1 0))))
            full-mantissa (insert-scaled-decimal rounded-mantissa k)
            append-zero (and (= k (count rounded-mantissa)) (nil? d))]
        (if (not incr-exp)
          (if w
            (let [len (+ (count full-mantissa) exp-width)
                  signed-len (if add-sign (inc len) len)
                  prepend-zero (and prepend-zero (not (= signed-len w)))
                  full-len (if prepend-zero (inc signed-len) signed-len)
                  append-zero (and append-zero (< full-len w))]
              (if (and (or (> full-len w) (and e (> (- exp-width 2) e)))
                       (:overflowchar params))
                (print* (apply str (repeat w (:overflowchar params))))
                (print* (str
                          (apply str
                                 (repeat
                                   (- w full-len (if append-zero 1 0))
                                   (:padchar params)))
                          (if add-sign (if (neg? arg) \- \+))
                          (if prepend-zero "0")
                          full-mantissa
                          (if append-zero "0")
                          scaled-exp-str))))
            (print* (str
                      (if add-sign (if (neg? arg) \- \+))
                      (if prepend-zero "0")
                      full-mantissa
                      (if append-zero "0")
                      scaled-exp-str)))
          (recur [rounded-mantissa (inc exp)]))))
    navigator))

;; the function to render ~G directives
;; This just figures out whether to pass the request off to ~F or ~E based
;; on the algorithm in CLtL.
;; TODO: support rationals. Back off to ~D/~A in the appropriate cases
;; TODO: refactor so that float-parts isn't called twice
(defn- general-float [params navigator offsets]
  (let [[arg _] (next-arg navigator)
        arg (convert-ratio arg)
        [mantissa exp] (float-parts (if (neg? arg) (- arg) arg))
        w (:w params)
        d (:d params)
        e (:e params)
        n (if (= arg 0.0) 0 (inc exp))
        ee (if e (+ e 2) 4)
        ww (if w (= w ee))
        d (if d d (max (count mantissa) (min n 7)))
        dd (- d n)]
    (if (<= 0 dd d)
      (let [navigator (fixed-float {:w ww, :d dd, :k 0,
                                    :overflowchar (:overflowchar params),
                                    :padchar (:padchar params), :at (:at params)}
                                   navigator offsets)]
        (print* (apply str (repeat ee \space)))
        navigator)
      (exponential-float params navigator offsets))))

;; the function to render ~$ directives
;; TODO: support rationals. Back off to ~D/~A in the appropriate cases
(defn- dollar-float [params navigator offsets]
  (let [[arg navigator] (next-arg navigator)
        [mantissa exp] (float-parts (Math/abs arg))
        d (:d params)  ; digits after the decimal
        n (:n params)  ; minimum digits before the decimal
        w (:w params)  ; minimum field width
        add-sign (or (:at params) (neg? arg))
        [rounded-mantissa scaled-exp expanded] (round-str mantissa exp d nil)
        fixed-repr (get-fixed rounded-mantissa (if expanded (inc scaled-exp) scaled-exp) d)
        full-repr (str (apply str (repeat (- n (.indexOf fixed-repr \.)) \0)) fixed-repr)
        full-len (+ (count full-repr) (if add-sign 1 0))]
    (print* (str
              (if (and (:colon params) add-sign) (if (neg? arg) \- \+))
              (apply str (repeat (- w full-len) (:padchar params)))
              (if (and (not (:colon params)) add-sign) (if (neg? arg) \- \+))
              full-repr))
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the '~[...~]' conditional construct in its
;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~[...~] without any modifiers chooses one of the clauses based on the param or
;; next argument
;; TODO check arg is positive int
(defn- choice-conditional [params arg-navigator offsets]
  (let [arg (:selector params)
        [arg navigator] (if arg [arg arg-navigator] (next-arg arg-navigator))
        clauses (:clauses params)
        clause (if (or (neg? arg) (>= arg (count clauses)))
                 (first (:else params))
                 (nth clauses arg))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~:[...~] with the colon reads the next argument treating it as a truth value
(defn- boolean-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg
                 (second clauses)
                 (first clauses))]
    (if clause
      (execute-sub-format clause navigator (:base-args params))
      navigator)))

;; ~@[...~] with the at sign executes the conditional if the next arg is not
;; nil/false without consuming the arg
(defn- check-arg-conditional [params arg-navigator offsets]
  (let [[arg navigator] (next-arg arg-navigator)
        clauses (:clauses params)
        clause (if arg (first clauses))]
    (if arg
      (if clause
        (execute-sub-format clause arg-navigator (:base-args params))
        arg-navigator)
      navigator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the '~{...~}' iteration construct in its
;; different flavors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~{...~} without any modifiers uses the next argument as an argument list that
;; is consumed by all the iterations
(defn- iterate-sublist [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])
        [arg-list navigator] (next-arg navigator)
        args (init-navigator arg-list)]
    (loop [count 0
           args args
           last-pos (int -1)]
      (if (and (not max-count) (= (:pos args) last-pos) (> count 1))
        ;; TODO get the offset in here and call format exception
        (throw (js/Error "%{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest args))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause args (:base-args params))]
          (if (= :up-arrow (first iter-result))
            navigator
            (recur (inc count) iter-result (:pos args))))))))

;; ~:{...~} with the colon treats the next argument as a list of sublists. Each of the
;; sublists is used as the arglist for a single iteration.
(defn- iterate-list-of-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])
        [arg-list navigator] (next-arg navigator)]
    (loop [count 0
           arg-list arg-list]
      (if (or (and (empty? arg-list)
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format
                            clause
                            (init-navigator (first arg-list))
                            (init-navigator (next arg-list)))]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) (next arg-list))))))))

;; ~@{...~} with the at sign uses the main argument list as the arguments to the iterations
;; is consumed by all the iterations
(defn- iterate-main-list [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])]
    (loop [count 0
           navigator navigator
           last-pos (int -1)]
      (if (and (not max-count) (= (:pos navigator) last-pos) (> count 1))
        ;; TODO get the offset in here and call format exception
        (throw (js/Error "%@{ construct not consuming any arguments: Infinite loop!")))
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [iter-result (execute-sub-format clause navigator (:base-args params))]
          (if (= :up-arrow (first iter-result))
            (second iter-result)
            (recur
              (inc count) iter-result (:pos navigator))))))))

;; ~@:{...~} with both colon and at sign uses the main argument list as a set of sublists, one
;; of which is consumed with each iteration
(defn- iterate-main-sublists [params navigator offsets]
  (let [max-count (:max-iterations params)
        param-clause (first (:clauses params))
        [clause navigator] (if (empty? param-clause)
                             (get-format-arg navigator)
                             [param-clause navigator])]
    (loop [count 0
           navigator navigator]
      (if (or (and (empty? (:rest navigator))
                   (or (not (:colon (:right-params params))) (> count 0)))
              (and max-count (>= count max-count)))
        navigator
        (let [[sublist navigator] (next-arg-or-nil navigator)
              iter-result (execute-sub-format clause (init-navigator sublist) navigator)]
          (if (= :colon-up-arrow (first iter-result))
            navigator
            (recur (inc count) navigator)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The '~< directive has two completely different meanings
;; in the '~<...~>' form it does justification, but with
;; ~<...~:>' it represents the logical block operation of the
;; pretty printer.
;;
;; Unfortunately, the current architecture decides what function
;; to call at form parsing time before the sub-clauses have been
;; folded, so it is left to run-time to make the decision.
;;
;; TODO: make it possible to make these decisions at compile-time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare format-logical-block)
(declare justify-clauses)

(defn- logical-block-or-justify [params navigator offsets]
  (if (:colon (:right-params params))
    (format-logical-block params navigator offsets)
    (justify-clauses params navigator offsets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the '~<...~>' justification directive
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- render-clauses [clauses navigator base-navigator]
  (loop [clauses clauses
         acc []
         navigator navigator]
    (if (empty? clauses)
      [acc navigator]
      (let [clause (first clauses)
            [iter-result result-str] (let [sb (StringBuffer.)]
                                       (binding [*out* (StringBufferWriter. sb)]
                                         [(execute-sub-format clause navigator base-navigator)
                                          (str sb)]))]
        (if (= :up-arrow (first iter-result))
          [acc (second iter-result)]
          (recur (next clauses) (conj acc result-str) iter-result))))))

;; TODO support for ~:; constructions
(defn- justify-clauses [params navigator offsets]
  (let [[[eol-str] new-navigator] (when-let [else (:else params)]
                                    (render-clauses else navigator (:base-args params)))
        navigator (or new-navigator navigator)
        [else-params new-navigator] (when-let [p (:else-params params)]
                                      (realize-parameter-list p navigator))
        navigator (or new-navigator navigator)
        min-remaining (or (first (:min-remaining else-params)) 0)
        max-columns (or (first (:max-columns else-params))
                        (get-max-column *out*))
        clauses (:clauses params)
        [strs navigator] (render-clauses clauses navigator (:base-args params))
        slots (max 1
                   (+ (dec (count strs)) (if (:colon params) 1 0) (if (:at params) 1 0)))
        chars (reduce + (map count strs))
        mincol (:mincol params)
        minpad (:minpad params)
        colinc (:colinc params)
        minout (+ chars (* slots minpad))
        result-columns (if (<= minout mincol)
                         mincol
                         (+ mincol (* colinc
                                      (+ 1 (quot (- minout mincol 1) colinc)))))
        total-pad (- result-columns chars)
        pad (max minpad (quot total-pad slots))
        extra-pad (- total-pad (* pad slots))
        pad-str (apply str (repeat pad (:padchar params)))]
    (if (and eol-str (> (+ (get-column (:base @@*out*)) min-remaining result-columns)
                        max-columns))
      (print* eol-str))
    (loop [slots slots
           extra-pad extra-pad
           strs strs
           pad-only (or (:colon params)
                        (and (= (count strs) 1) (not (:at params))))]
      (if (seq strs)
        (do
          (print* (str (if (not pad-only) (first strs))
                       (if (or pad-only (next strs) (:at params)) pad-str)
                       (if (pos? extra-pad) (:padchar params))))
          (recur
            (dec slots)
            (dec extra-pad)
            (if pad-only strs (next strs))
            false))))
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for case modification with ~(...~).
;;; We do this by wrapping the underlying writer with
;;; a special writer to do the appropriate modification. This
;;; allows us to support arbitrary-sized output and sources
;;; that may block.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- downcase-writer
  "Returns a proxy that wraps writer, converting all characters to lower case"
  [writer]
  (reify
    IWriter
    (-flush [_] (-flush writer))
    (-write
      ;;no multi-arity, not sure of importance
      #_([^chars cbuf ^Integer off ^Integer len]
             (.write writer cbuf off len))
      [this x]
      (condp = (type x)
        js/String
        (let [s x]
          (-write writer (string/lower-case s)))

        js/Number
        (let [c x]
          ;;TODO need to enfore integers only?
          (-write writer (string/lower-case (char c))))))))

(defn- upcase-writer
  "Returns a proxy that wraps writer, converting all characters to upper case"
  [writer]
  (reify
    IWriter
    (-flush [_] (-flush writer))
    (-write
      ;;no multi-arity, not sure of importance
      #_([^chars cbuf ^Integer off ^Integer len]
             (.write writer cbuf off len))
      [this x]
      (condp = (type x)
        js/String
        (let [s x]
          (-write writer (string/upper-case s)))

        js/Number
        (let [c x]
          ;;TODO need to enfore integers only?
          (-write writer (string/upper-case (char c))))))))

;;TODO: This is an oversimplied version. Needs to be fully implemented
(defn- is-letter? [s]
  (boolean
    (#{"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
       "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"}
      s)))

(defn- capitalize-string
  "Capitalizes the words in a string. If first? is false, don't capitalize the
                                      first character of the string even if it's a letter."
  [s first?]
  (let [f (first s)
        s (if (and first? f (is-letter? f))
            (str (string/upper-case f) (subs s 1))
            s)]
    (apply str
           (first
             (consume
               (fn [s]
                 (if (empty? s)
                   [nil nil]
                   (let [m (.exec (js/RegExp "\\W\\w" "g") s)
                         offset (and m (inc (.-index m)))]
                     (if offset
                       [(str (subs s 0 offset)
                             (string/upper-case (nth s offset)))
                        (subs s (inc offset))]
                       [s nil]))))
               s)))))

;;TODO: This is an oversimplied version. Needs to be fully implemented
(defn- is-whitespace? [s]
  (boolean
    (#{\space \newline} s)))

(defn- capitalize-word-writer
  "Returns a proxy that wraps writer, capitalizing all words"
  [writer]
  (let [last-was-whitespace? (atom true)]
    (reify
      IWriter
      (-flush [_] (-flush writer))
      (-write
        ;;no multi-arity
        #_([^chars cbuf ^Integer off ^Integer len]
               (.write writer cbuf off len))
        [this x]
        (condp = (type x)
          js/String
          (let [s x]
            (-write writer
                    (capitalize-string (.toLowerCase s) @last-was-whitespace?))
            (when (pos? (.-length s))
              (reset! last-was-whitespace? (is-whitespace? (nth s (dec (count s)))))))

          js/Number
          (let [c (char x)]
            (let [mod-c (if @last-was-whitespace? (string/upper-case c) c)]
              (-write writer mod-c)
              (reset! last-was-whitespace? (is-whitespace? c)))))))))

(defn- init-cap-writer
  "Returns a proxy that wraps writer, capitalizing the first word"
  [writer]
  (let [capped (atom false)]
    (reify
      IWriter
      (-flush [_] (-flush writer))
      (-write
        ;;no multi-arity
        #_([^chars cbuf ^Integer off ^Integer len]
                    (.write writer cbuf off len))
        [this x]
        (condp = (type x)
          js/String
          (let [s (string/lower-case x)]
            (if (not @capped)
              (let [m (.exec (js/RegExp "\\S" "g") s)
                    offset (and m (inc (.-index m)))]
                (if offset
                  (do (-write writer
                              (str (subs s 0 offset)
                                   (string/upper-case (nth s offset))
                                   (string/lower-case (subs s (inc offset)))))
                      (reset! capped true))
                  (-write writer s)))
              (-write writer (string/lower-case s))))

          js/Number
          (let [c (char x)]
            (if (and (not @capped) (is-letter? c))
              (do
                (reset! capped true)
                (-write writer (string/upper-case c)))
              (-write writer (string/lower-case c)))))))))

(defn- modify-case [make-writer params navigator offsets]
  (let [clause (first (:clauses params))]
    (binding [*out* (make-writer *out*)]
      (execute-sub-format clause navigator (:base-args params)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If necessary, wrap the writer in a PrettyWriter object
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO update this comment to show correct way to print
(defn get-pretty-writer
  "Returns the IWriter passed in wrapped in a pretty writer proxy, unless it's
already a pretty writer. Generally, it is unnecessary to call this function, since pprint,
write, and cl-format all call it if they need to. However if you want the state to be
preserved across calls, you will want to wrap them with this.

For example, when you want to generate column-aware output with multiple calls to cl-format,
do it like in this example:

    (defn print-table [aseq column-width]
      (binding [*out* (get-pretty-writer *out*)]
        (doseq [row aseq]
          (doseq [col row]
            (cl-format true \"~4D~7,vT\" col column-width))
          (prn))))

Now when you run:

    user> (print-table (map #(vector % (* % %) (* % % %)) (range 1 11)) 8)

It prints a table of squares and cubes for the numbers from 1 to 10:

       1      1       1
       2      4       8
       3      9      27
       4     16      64
       5     25     125
       6     36     216
       7     49     343
       8     64     512
       9     81     729
      10    100    1000"
  [writer]
  (if (pretty-writer? writer)
    writer
    (pretty-writer writer *print-right-margin* *print-miser-width*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for column-aware operations ~&, ~T
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fresh-line
  "Make a newline if *out* is not already at the beginning of the line. If *out* is
not a pretty writer (which keeps track of columns), this function always outputs a newline."
  {:added "1.2"}
  []
  (if (satisfies? IDeref *out*)
    (if (not (= 0 (get-column (:base @@*out*))))
      (prn*))
    (prn*)))

(defn- absolute-tabulation [params navigator offsets]
  (let [colnum (:colnum params)
        colinc (:colinc params)
        current (get-column (:base @@*out*))
        space-count (cond
                      (< current colnum) (- colnum current)
                      (= colinc 0) 0
                      :else (- colinc (rem (- current colnum) colinc)))]
    (print* (apply str (repeat space-count \space))))
  navigator)

(defn- relative-tabulation [params navigator offsets]
  (let [colrel (:colnum params)
        colinc (:colinc params)
        start-col (+ colrel (get-column (:base @@*out*)))
        offset (if (pos? colinc) (rem start-col colinc) 0)
        space-count (+ colrel (if (= 0 offset) 0 (- colinc offset)))]
    (print* (apply str (repeat space-count \space))))
  navigator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support for accessing the pretty printer from a format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: support ~@; per-line-prefix separator
;; TODO: get the whole format wrapped so we can start the lb at any column
(defn- format-logical-block [params navigator offsets]
  (let [clauses (:clauses params)
        clause-count (count clauses)
        prefix (cond
                 (> clause-count 1) (:string (:params (first (first clauses))))
                 (:colon params) "(")
        body (nth clauses (if (> clause-count 1) 1 0))
        suffix (cond
                 (> clause-count 2) (:string (:params (first (nth clauses 2))))
                 (:colon params) ")")
        [arg navigator] (next-arg navigator)]
    (pprint-logical-block :prefix prefix :suffix suffix
      (execute-sub-format
        body
        (init-navigator arg)
        (:base-args params)))
    navigator))

(defn- set-indent [params navigator offsets]
  (let [relative-to (if (:colon params) :current :block)]
    (pprint-indent relative-to (:n params))
    navigator))

;;; TODO: support ~:T section options for ~T
(defn- conditional-newline [params navigator offsets]
  (let [kind (if (:colon params)
               (if (:at params) :mandatory :fill)
               (if (:at params) :miser :linear))]
    (pprint-newline kind)
    navigator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The table of directives we support, each with its params,
;;; properties, and the compilation function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defdirectives
  (\A
    [:mincol [0 js/Number] :colinc [1 js/Number] :minpad [0 js/Number] :padchar [\space js/String]]
    #{:at :colon :both} {}
    #(format-ascii print-str %1 %2 %3))

  (\S
    [:mincol [0 js/Number] :colinc [1 js/Number] :minpad [0 js/Number] :padchar [\space js/String]]
    #{:at :colon :both} {}
    #(format-ascii pr-str %1 %2 %3))

  (\D
    [:mincol [0 js/Number] :padchar [\space js/String] :commachar [\, js/String]
     :commainterval [3 js/Number]]
    #{:at :colon :both} {}
    #(format-integer 10 %1 %2 %3))

  (\B
    [:mincol [0 js/Number] :padchar [\space js/String] :commachar [\, js/String]
     :commainterval [3 js/Number]]
    #{:at :colon :both} {}
    #(format-integer 2 %1 %2 %3))

  (\O
    [:mincol [0 js/Number] :padchar [\space js/String] :commachar [\, js/String]
     :commainterval [3 js/Number]]
    #{:at :colon :both} {}
    #(format-integer 8 %1 %2 %3))

  (\X
    [:mincol [0 js/Number] :padchar [\space js/String] :commachar [\, js/String]
     :commainterval [3 js/Number]]
    #{:at :colon :both} {}
    #(format-integer 16 %1 %2 %3))

  (\R
    [:base [nil js/Number] :mincol [0 js/Number] :padchar [\space js/String] :commachar [\, js/String]
     :commainterval [3 js/Number]]
    #{:at :colon :both} {}
    (do
      (cond                          ; ~R is overloaded with bizareness
        (first (:base params))     #(format-integer (:base %1) %1 %2 %3)
        (and (:at params) (:colon params))   #(format-old-roman %1 %2 %3)
        (:at params)               #(format-new-roman %1 %2 %3)
        (:colon params)            #(format-ordinal-english %1 %2 %3)
        true                       #(format-cardinal-english %1 %2 %3))))

  (\P
    []
    #{:at :colon :both} {}
    (fn [params navigator offsets]
      (let [navigator (if (:colon params) (relative-reposition navigator -1) navigator)
            strs (if (:at params) ["y" "ies"] ["" "s"])
            [arg navigator] (next-arg navigator)]
        (print (if (= arg 1) (first strs) (second strs)))
        navigator)))

  (\C
    [:char-format [nil js/String]]
    #{:at :colon :both} {}
    (cond
      (:colon params) pretty-character
      (:at params) readable-character
      :else plain-character))

  (\F
    [:w [nil js/Number] :d [nil js/Number] :k [0 js/Number] :overflowchar [nil js/String]
     :padchar [\space js/String]]
    #{:at} {}
    fixed-float)

  (\E
    [:w [nil js/Number] :d [nil js/Number] :e [nil js/Number] :k [1 js/Number]
     :overflowchar [nil js/String] :padchar [\space js/String]
     :exponentchar [nil js/String]]
    #{:at} {}
    exponential-float)

  (\G
    [:w [nil js/Number] :d [nil js/Number] :e [nil js/Number] :k [1 js/Number]
     :overflowchar [nil js/String] :padchar [\space js/String]
     :exponentchar [nil js/String]]
    #{:at} {}
    general-float)

  (\$
    [:d [2 js/Number] :n [1 js/Number] :w [0 js/Number] :padchar [\space js/String]]
    #{:at :colon :both} {}
    dollar-float)

  (\%
    [:count [1 js/Number]]
    #{} {}
    (fn [params arg-navigator offsets]
      (dotimes [i (:count params)]
        (prn*))
      arg-navigator))

  (\&
    [:count [1 js/Number]]
    #{:pretty} {}
    (fn [params arg-navigator offsets]
      (let [cnt (:count params)]
        (if (pos? cnt) (fresh-line))
        (dotimes [i (dec cnt)]
          (prn*)))
      arg-navigator))

  (\|
    [:count [1 js/Number]]
    #{} {}
    (fn [params arg-navigator offsets]
      (dotimes [i (:count params)]
        (print* \formfeed))
      arg-navigator))

  (\~
    [:n [1 js/Number]]
    #{} {}
    (fn [params arg-navigator offsets]
      (let [n (:n params)]
        (print* (apply str (repeat n \~)))
        arg-navigator)))

  (\newline ;; Whitespace supression is handled in the compilation loop
    []
    #{:colon :at} {}
    (fn [params arg-navigator offsets]
      (if (:at params)
        (prn*))
      arg-navigator))

  (\T
    [:colnum [1 js/Number] :colinc [1 js/Number]]
    #{:at :pretty} {}
    (if (:at params)
      #(relative-tabulation %1 %2 %3)
      #(absolute-tabulation %1 %2 %3)))

  (\*
    [:n [1 js/Number]]
    #{:colon :at} {}
    (fn [params navigator offsets]
      (let [n (:n params)]
        (if (:at params)
          (absolute-reposition navigator n)
          (relative-reposition navigator (if (:colon params) (- n) n))))))

  (\?
    []
    #{:at} {}
    (if (:at params)
      (fn [params navigator offsets]     ; args from main arg list
        (let [[subformat navigator] (get-format-arg navigator)]
          (execute-sub-format subformat navigator (:base-args params))))
      (fn [params navigator offsets]     ; args from sub-list
        (let [[subformat navigator] (get-format-arg navigator)
              [subargs navigator] (next-arg navigator)
              sub-navigator (init-navigator subargs)]
          (execute-sub-format subformat sub-navigator (:base-args params))
          navigator))))

  (\(
    []
    #{:colon :at :both} {:right \), :allows-separator nil, :else nil}
    (let [mod-case-writer (cond
                            (and (:at params) (:colon params))
                            upcase-writer

                            (:colon params)
                            capitalize-word-writer

                            (:at params)
                            init-cap-writer

                            :else
                            downcase-writer)]
      #(modify-case mod-case-writer %1 %2 %3)))

  (\) [] #{} {} nil)

  (\[
    [:selector [nil js/Number]]
    #{:colon :at} {:right \], :allows-separator true, :else :last}
    (cond
      (:colon params)
      boolean-conditional

      (:at params)
      check-arg-conditional

      true
      choice-conditional))

  (\; [:min-remaining [nil js/Number] :max-columns [nil js/Number]]
    #{:colon} {:separator true} nil)

  (\] [] #{} {} nil)

  (\{
    [:max-iterations [nil js/Number]]
    #{:colon :at :both} {:right \}, :allows-separator false}
    (cond
      (and (:at params) (:colon params))
      iterate-main-sublists

      (:colon params)
      iterate-list-of-sublists

      (:at params)
      iterate-main-list

      true
      iterate-sublist))

  (\} [] #{:colon} {} nil)

  (\<
    [:mincol [0 js/Number] :colinc [1 js/Number] :minpad [0 js/Number] :padchar [\space js/String]]
    #{:colon :at :both :pretty} {:right \>, :allows-separator true, :else :first}
    logical-block-or-justify)

  (\> [] #{:colon} {} nil)

  ;; TODO: detect errors in cases where colon not allowed
  (\^ [:arg1 [nil js/Number] :arg2 [nil js/Number] :arg3 [nil js/Number]]
    #{:colon} {}
    (fn [params navigator offsets]
      (let [arg1 (:arg1 params)
            arg2 (:arg2 params)
            arg3 (:arg3 params)
            exit (if (:colon params) :colon-up-arrow :up-arrow)]
        (cond
          (and arg1 arg2 arg3)
          (if (<= arg1 arg2 arg3) [exit navigator] navigator)

          (and arg1 arg2)
          (if (= arg1 arg2) [exit navigator] navigator)

          arg1
          (if (= arg1 0) [exit navigator] navigator)

          true     ; TODO: handle looking up the arglist stack for info
          (if (if (:colon params)
                (empty? (:rest (:base-args params)))
                (empty? (:rest navigator)))
            [exit navigator] navigator)))))

  (\W
    []
    #{:at :colon :both :pretty} {}
    (if (or (:at params) (:colon params))
      (let [bindings (concat
                       (if (:at params) [:level nil :length nil] [])
                       (if (:colon params) [:pretty true] []))]
        (fn [params navigator offsets]
          (let [[arg navigator] (next-arg navigator)]
            (if (apply write arg bindings)
              [:up-arrow navigator]
              navigator))))
      (fn [params navigator offsets]
        (let [[arg navigator] (next-arg navigator)]
          (if (write-out arg)
            [:up-arrow navigator]
            navigator)))))

  (\_
    []
    #{:at :colon :both} {}
    conditional-newline)

  (\I
    [:n [0 js/Number]]
    #{:colon} {}
    set-indent)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code to manage the parameters and flags associated with each
;; directive in the format string.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:private true}
     param-pattern #"^([vV]|#|('.)|([+-]?\d+)|(?=,))")

(def ^{:private true}
     special-params #{:parameter-from-args :remaining-arg-count})

(defn- extract-param [[s offset saw-comma]]
  (let [m (js/RegExp. (.-source param-pattern) "g")
        param (.exec m s)]
    (if param
      (let [token-str (ffirst param)
            remainder (subs s (.-lastIndex m))
            new-offset (+ offset (.-lastIndex m))]
        (if (not (= \, (nth remainder 0)))
          [ [token-str offset] [remainder new-offset false]]
          [ [token-str offset] [(subs remainder 1) (inc new-offset) true]]))
      (if saw-comma
        (format-error "Badly formed parameters in format directive" offset)
        [ nil [s offset]]))))

(defn- extract-params [s offset]
  (consume extract-param [s offset false]))

(defn- translate-param
  "Translate the string representation of a param to the internalized
                                      representation"
  [[p offset]]
  [(cond
     (= (.-length p) 0) nil
     (and (= (.-length p) 1) (contains? #{\v \V} (nth p 0))) :parameter-from-args
     (and (= (.-length p) 1) (= \# (nth p 0))) :remaining-arg-count
     (and (= (.-length p) 2) (= \' (nth p 0))) (nth p 1)
     true (js/parseInt p))
   offset])

(def ^{:private true}
     flag-defs {\: :colon, \@ :at})

(defn- extract-flags [s offset]
  (consume
    (fn [[s offset flags]]
      (if (empty? s)
        [nil [s offset flags]]
        (let [flag (get flag-defs (first s))]
          (if flag
            (if (contains? flags flag)
              (format-error
                (str "Flag \"" (first s) "\" appears more than once in a directive")
                offset)
              [true [(subs s 1) (inc offset) (assoc flags flag [true offset])]])
            [nil [s offset flags]]))))
    [s offset {}]))

(defn- check-flags [def flags]
  (let [allowed (:flags def)]
    (if (and (not (:at allowed)) (:at flags))
      (format-error (str "\"@\" is an illegal flag for format directive \"" (:directive def) "\"")
                    (nth (:at flags) 1)))
    (if (and (not (:colon allowed)) (:colon flags))
      (format-error (str "\":\" is an illegal flag for format directive \"" (:directive def) "\"")
                    (nth (:colon flags) 1)))
    (if (and (not (:both allowed)) (:at flags) (:colon flags))
      (format-error (str "Cannot combine \"@\" and \":\" flags for format directive \""
                         (:directive def) "\"")
                    (min (nth (:colon flags) 1) (nth (:at flags) 1))))))

(defn- map-params
  "Takes a directive definition and the list of actual parameters and
a map of flags and returns a map of the parameters and flags with defaults
filled in. We check to make sure that there are the right types and number
of parameters as well."
  [def params flags offset]
  (check-flags def flags)
  (if (> (count params) (count (:params def)))
    (format-error
      (cl-format
        nil
        "Too many parameters for directive \"~C\": ~D~:* ~[were~;was~:;were~] specified but only ~D~:* ~[are~;is~:;are~] allowed"
        (:directive def) (count params) (count (:params def)))
      (second (first params))))
  (doall
    (map #(let [val (first %1)]
           (if (not (or (nil? val) (contains? special-params val)
                        (= (second (second %2)) (type val))))
             (format-error (str "Parameter " (name (first %2))
                                " has bad type in directive \"" (:directive def) "\": "
                                (type val))
                           (second %1))) )
         params (:params def)))

  (merge                                ; create the result map
    (into (array-map) ; start with the default values, make sure the order is right
          (reverse (for [[name [default]] (:params def)] [name [default offset]])))
    (reduce #(apply assoc %1 %2) {} (filter #(first (nth % 1)) (zipmap (keys (:params def)) params))) ; add the specified parameters, filtering out nils
    flags))                                ; and finally add the flags

(defn- compile-directive [s offset]
  (let [[raw-params [rest offset]] (extract-params s offset)
        [_ [rest offset flags]] (extract-flags rest offset)
        directive (first rest)
        def (get directive-table (string/upper-case directive))
        params (if def (map-params def (map translate-param raw-params) flags offset))]
    (if (not directive)
      (format-error "Format string ended in the middle of a directive" offset))
    (if (not def)
      (format-error (str "Directive \"" directive "\" is undefined") offset))
    [(compiled-directive. ((:generator-fn def) params offset) def params offset)
     (let [remainder (subs rest 1)
           offset (inc offset)
           trim? (and (= \newline (:directive def))
                      (not (:colon params)))
           trim-count (if trim? (prefix-count remainder [\space \tab]) 0)
           remainder (subs remainder trim-count)
           offset (+ offset trim-count)]
       [remainder offset])]))

(defn- compile-raw-string [s offset]
  (compiled-directive. (fn [_ a _] (print s) a) nil {:string s} offset))

(defn- right-bracket [this] (:right (:bracket-info (:def this))))

(defn- separator? [this] (:separator (:bracket-info (:def this))))

(defn- else-separator? [this]
  (and (:separator (:bracket-info (:def this)))
       (:colon (:params this))))

(declare collect-clauses)

(defn- process-bracket [this remainder]
  (let [[subex remainder] (collect-clauses (:bracket-info (:def this))
                                           (:offset this) remainder)]
    [(compiled-directive.
       (:func this) (:def this)
       (merge (:params this) (tuple-map subex (:offset this)))
       (:offset this))
     remainder]))

(defn- process-clause [bracket-info offset remainder]
  (consume
    (fn [remainder]
      (if (empty? remainder)
        (format-error "No closing bracket found." offset)
        (let [this (first remainder)
              remainder (next remainder)]
          (cond
            (right-bracket this)
            (process-bracket this remainder)

            (= (:right bracket-info) (:directive (:def this)))
            [ nil [:right-bracket (:params this) nil remainder]]

            (else-separator? this)
            [nil [:else nil (:params this) remainder]]

            (separator? this)
            [nil [:separator nil nil remainder]] ;; TODO: check to make sure that there are no params on ~;

            true
            [this remainder]))))
    remainder))

(defn- collect-clauses [bracket-info offset remainder]
  (second
    (consume
      (fn [[clause-map saw-else remainder]]
        (let [[clause [type right-params else-params remainder]]
              (process-clause bracket-info offset remainder)]
          (cond
            (= type :right-bracket)
            [nil [(merge-with concat clause-map
                              {(if saw-else :else :clauses) [clause]
                               :right-params right-params})
                  remainder]]

            (= type :else)
            (cond
              (:else clause-map)
              (format-error "Two else clauses (\"~:;\") inside bracket construction." offset)

              (not (:else bracket-info))
              (format-error "An else clause (\"~:;\") is in a bracket type that doesn't support it."
                            offset)

              (and (= :first (:else bracket-info)) (seq (:clauses clause-map)))
              (format-error
                "The else clause (\"~:;\") is only allowed in the first position for this directive."
                offset)

              true         ; if the ~:; is in the last position, the else clause
              ; is next, this was a regular clause
              (if (= :first (:else bracket-info))
                [true [(merge-with concat clause-map {:else [clause] :else-params else-params})
                       false remainder]]
                [true [(merge-with concat clause-map {:clauses [clause]})
                       true remainder]]))

            (= type :separator)
            (cond
              saw-else
              (format-error "A plain clause (with \"~;\") follows an else clause (\"~:;\") inside bracket construction." offset)

              (not (:allows-separator bracket-info))
              (format-error "A separator (\"~;\") is in a bracket type that doesn't support it."
                            offset)

              true
              [true [(merge-with concat clause-map {:clauses [clause]})
                     false remainder]]))))
      [{:clauses []} false remainder])))

(defn- process-nesting
  "Take a linearly compiled format and process the bracket directives to give it
   the appropriate tree structure"
  [format]
  (first
    (consume
      (fn [remainder]
        (let [this (first remainder)
              remainder (next remainder)
              bracket (:bracket-info (:def this))]
          (if (:right bracket)
            (process-bracket this remainder)
            [this remainder])))
      format)))

(defn- compile-format
  "Compiles format-str into a compiled format which can be used as an argument
to cl-format just like a plain format string. Use this function for improved
performance when you're using the same format string repeatedly"
  [format-str]
  (binding [*format-str* format-str]
    (process-nesting
      (first
        (consume
          (fn [[s offset]]
            (if (empty? s)
              [nil s]
              (let [tilde (.indexOf s \~)]
                (cond
                  (neg? tilde) [(compile-raw-string s offset) ["" (+ offset (.-length s))]]
                  (zero? tilde) (compile-directive (subs s 1) (inc offset))
                  true
                  [(compile-raw-string (subs s 0 tilde) offset) [(subs s tilde) (+ tilde offset)]]))))
          [format-str 0])))))

(defn- needs-pretty
  "determine whether a given compiled format has any directives that depend on the
column number or pretty printing"
  [format]
  (loop [format format]
    (if (empty? format)
      false
      (if (or (:pretty (:flags (:def (first format))))
              (some needs-pretty (first (:clauses (:params (first format)))))
              (some needs-pretty (first (:else (:params (first format))))))
        true
        (recur (next format))))))

(defn- execute-format
  "Executes the format with the arguments."
  {:skip-wiki true}
  ([stream format args]
   (let [sb (StringBuffer.)
         real-stream (cond
                       (not stream) (StringBufferWriter. sb)
                       (true? stream) *out*
                       :else stream)
         wrapped-stream (if (and (needs-pretty format)
                                 (not (pretty-writer? real-stream)))
                          (get-pretty-writer real-stream)
                          real-stream)]
     (binding [*out* wrapped-stream]
       (try
         (execute-format format args)
         (finally
           (if-not (identical? real-stream wrapped-stream)
             (-flush wrapped-stream))))
       (if (not stream) (str sb)))))
  ([format args]
   (map-passing-context
     (fn [element context]
       (if (abort? context)
         [nil context]
         (let [[params args] (realize-parameter-list
                               (:params element) context)
               [params offsets] (unzip-map params)
               params (assoc params :base-args args)]
           [nil (apply (:func element) [params args offsets])])))
     args
     format)
   nil))

;;; This is a bad idea, but it prevents us from leaking private symbols
;;; This should all be replaced by really compiled formats anyway.
(def ^{:private true} cached-compile (memoize compile-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pprint/dispatch.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti
  simple-dispatch
  "The pretty print dispatch function for simple data structure format."
  (fn [obj]
    (cond
      (list? obj) :list
      (map? obj) :map
      (vector? obj) :vector
      (nil? obj) nil
      :default :default)))

(defmethod simple-dispatch :list [alis]
  (pprint-logical-block :prefix "(" :suffix ")"
    (print-length-loop [alis (seq alis)]
      (when alis
        (write-out (first alis))
        (when (next alis)
          (-write *out* " ")
          (pprint-newline :linear)
          (recur (next alis)))))))

(defmethod simple-dispatch :vector [avec]
  (pprint-logical-block :prefix "[" :suffix "]"
    (print-length-loop [aseq (seq avec)]
      (when aseq
        (write-out (first aseq))
        (when (next aseq)
          (-write *out* " ")
          (pprint-newline :linear)
          (recur (next aseq)))))))

(defmethod simple-dispatch :map [amap]
  (pprint-logical-block :prefix "{" :suffix "}"
    (print-length-loop [aseq (seq amap)]
      (when aseq
        (pprint-logical-block
          (write-out (ffirst aseq))
          (-write *out* " ")
          (pprint-newline :linear)
          (set! *current-length* 0)   ;always print both parts of the [k v] pair
          (write-out (fnext (first aseq))))
        (when (next aseq)
          (-write *out* ", ")
          (pprint-newline :linear)
          (recur (next aseq)))))))

#_(defmethod simple-dispatch cljs.core/PersistentTreeSet [aset]
  ((formatter-out "~<#{~;~@{~w~^ ~:_~}~;}~:>") aset))

(defmethod simple-dispatch :default [obj]
  ;;TODO: Update to handle arrays (?) and suppressing namespaces
  (-write *out* (pr-str obj)))

(set-pprint-dispatch simple-dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; pprint/print_table.clj
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

