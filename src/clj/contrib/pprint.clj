(ns clj.contrib.pprint
  (:refer-clojure :exclude [deftype])
  (:require [clojure.walk :as walk]))

(defmacro getf
  "Get the value of the field a named by the argument (which should be a keyword)."
  [sym]
  `(~sym @@~'this))

(defmacro setf
  "Set the value of the field SYM to NEW-VAL"
  [sym new-val]
  `(swap! @~'this assoc ~sym ~new-val))

(defmacro deftype
  [type-name & fields]
  (let [name-str (name type-name)
        fields (concat [:type-tag] fields)
        fields (map (comp symbol name) fields)]
    `(do
       (defrecord ~type-name [~@fields])
       (defn- ~(symbol (str "make-" name-str))
         [& vals#]
         (~(symbol (str type-name ".")) ~(keyword name-str) (nth vals# 0) (nth vals# 1) (nth vals# 2) (nth vals# 3) (nth vals# 4))
         ;;Temporary
         #_(condp = (count vals#)
           ;3 (~(symbol (str type-name ".")) ~(keyword name-str) (nth vals# 0) (nth vals# 1) (nth vals# 2))
           ;4 (~(symbol (str type-name ".")) ~(keyword name-str) (nth vals# 0) (nth vals# 1) (nth vals# 2) (nth vals# 3))
           5
           ))
       (defn- ~(symbol (str name-str "?")) [x#] (= (:type-tag x#) ~(keyword name-str))))))

(defn- parse-lb-options [opts body]
  (loop [body body
         acc []]
    (if (opts (first body))
      (recur (drop 2 body) (concat acc (take 2 body)))
      [(apply hash-map acc) body])))

(defmacro pprint-logical-block
  "Execute the body as a pretty printing logical block with output to *out* which
  must be a pretty printing writer. When used from pprint or cl-format, this can be
  assumed.

  This function is intended for use when writing custom dispatch functions.

  Before the body, the caller can optionally specify options: :prefix, :per-line-prefix
  and :suffix."
  [& args]
  (let [[options body] (parse-lb-options #{:prefix :per-line-prefix :suffix} args)]
    `(do (if (cljs.contrib.pprint/level-exceeded)
           (~'-write cljs.contrib.pprint/*out* "#")
           (do
             (binding [cljs.contrib.pprint/*current-level* (inc cljs.contrib.pprint/*current-level*)
                       cljs.contrib.pprint/*current-length* 0]
               (cljs.contrib.pprint/start-block cljs.contrib.pprint/*out*
                                        ~(:prefix options)
                                        ~(:per-line-prefix options)
                                        ~(:suffix options))
               ~@body
               (cljs.contrib.pprint/end-block cljs.contrib.pprint/*out*))))
         nil)))

(defn- pll-mod-body [var-sym body]
  (letfn [(inner [form]
                 (if (seq? form)
                   (let [form (macroexpand form)]
                     (condp = (first form)
                       'loop* form
                       'recur (concat `(recur (inc ~var-sym)) (rest form))
                       (walk/walk inner identity form)))
                   form))]
    (walk/walk inner identity body)))

(defmacro print-length-loop
  "A version of loop that iterates at most *print-length* times. This is designed
  for use in pretty-printer dispatch functions."
  [bindings & body]
  (let [count-var (gensym "length-count")
        mod-body (pll-mod-body count-var body)]
    `(loop ~(apply vector count-var 0 bindings)
       (if (or (not cljs.core/*print-length*) (< ~count-var cljs.core/*print-length*))
         (do ~@mod-body)
         (~'-write cljs.contrib.pprint/*out* "...")))))

(defmacro with-pretty-writer [base-writer & body]
  `(let [base-writer# ~base-writer
         new-writer# (not (cljs.contrib.pprint/pretty-writer? base-writer#))]
     (binding [cljs.contrib.pprint/*out* (if new-writer#
                                   (cljs.contrib.pprint/make-pretty-writer base-writer#
                                                                   cljs.contrib.pprint/*print-right-margin*
                                                                   cljs.contrib.pprint/*print-miser-width*)
                                   base-writer#)]
       ~@body
       (~'-ppflush cljs.contrib.pprint/*out*))))

(defn- process-directive-table-element [[char params flags bracket-info & generator-fn]]
  [char,
   {:directive char,
    :params `(array-map ~@params),
    :flags flags,
    :bracket-info bracket-info,
    :generator-fn (concat '(fn [params offset]) generator-fn)}])

(defmacro ^{:private true}
  defdirectives
  [& directives]
  `(def ^{:private true}
        ~'directive-table (hash-map ~@(mapcat process-directive-table-element directives))))

(defmacro formatter
  "Makes a function which can directly run format-in. The function is
fn [stream & args] ... and returns nil unless the stream is nil (meaning
output to a string) in which case it returns the resulting string.

format-in can be either a control string or a previously compiled format."
  {:added "1.2"}
  [format-in]
  `(let [format-in# ~format-in
         my-c-c# #'cljs.contrib.pprint/cached-compile
         my-e-f# #'cljs.contrib.pprint/execute-format
         my-i-n# #'cljs.contrib.pprint/init-navigator
         cf# (if (string? format-in#) (my-c-c# format-in#) format-in#)]
     (fn [stream# & args#]
       (let [navigator# (my-i-n# args#)]
         (my-e-f# stream# cf# navigator#)))))

(defmacro formatter-out
  "Makes a function which can directly run format-in. The function is
fn [& args] ... and returns nil. This version of the formatter macro is
designed to be used with *out* set to an appropriate Writer. In particular,
this is meant to be used as part of a pretty printer dispatch method.

format-in can be either a control string or a previously compiled format."
  {:added "1.2"}
  [format-in]
  `(let [format-in# ~format-in
         cf# (if (string? format-in#) (#'cljs.contrib.pprint/cached-compile format-in#) format-in#)]
     (fn [& args#]
       (let [navigator# (#'cljs.contrib.pprint/init-navigator args#)]
         (#'cljs.contrib.pprint/execute-format cf# navigator#)))))

