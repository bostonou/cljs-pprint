(ns cljs.contrib.pprint-test
  (:require
    [cemerick.cljs.test :as t]
    [cljs.contrib.pprint :refer [pprint]])
  (:require-macros
    [cemerick.cljs.test :refer [deftest is]]
    [clj.contrib.pprint-test :refer [simple-tests]]))

(simple-tests print-length-tests
  (binding [*print-length* 1] (with-out-str (pprint '(a b c d e f))))
  "(a ...)\n"
  (binding [*print-length* 2] (with-out-str (pprint '(a b c d e f))))
  "(a b ...)\n"
  (binding [*print-length* 6] (with-out-str (pprint '(a b c d e f))))
  "(a b c d e f)\n"
  (binding [*print-length* 8] (with-out-str (pprint '(a b c d e f))))
  "(a b c d e f)\n"

  (binding [*print-length* 1] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 ...]\n"
  (binding [*print-length* 2] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 ...]\n"
  (binding [*print-length* 6] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 3 4 5 6]\n"
  (binding [*print-length* 8] (with-out-str (pprint [1 2 3 4 5 6])))
  "[1 2 3 4 5 6]\n"

  (binding [*print-length* 1] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  "#{1 ...}\n"
  #_(binding [*print-length* 2] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  #_"#{1 2 ...}\n"
  #_(binding [*print-length* 6] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  #_"#{1 2 3 4 5 6}\n"
  #_(binding [*print-length* 8] (with-out-str (pprint (sorted-set 1 2 3 4 5 6))))
  #_"#{1 2 3 4 5 6}\n"

  (binding [*print-length* 1] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, ...}\n"
  (binding [*print-length* 2] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, ...}\n"
  (binding [*print-length* 6] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, 5 6, 7 8, 9 10, 11 12}\n"
  (binding [*print-length* 8] (with-out-str (pprint (sorted-map 1 2, 3 4, 5 6, 7 8, 9 10, 11 12))))
  "{1 2, 3 4, 5 6, 7 8, 9 10, 11 12}\n"

  #_(binding [*print-length* 1] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  #_"[1, ...]\n"
  #_(binding [*print-length* 2] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  #_"[1, 2, ...]\n"
  #_(binding [*print-length* 6] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  #_"[1, 2, 3, 4, 5, 6]\n"
  #_(binding [*print-length* 8] (with-out-str (pprint (int-array [1 2 3 4 5 6]))))
  #_"[1, 2, 3, 4, 5, 6]\n"
  )

(simple-tests print-margin-tests
  (binding [cljs.contrib.pprint/*print-right-margin* 20]
    (with-out-str (pprint (sorted-map 1 (sorted-map 12345 123456), 3 (sorted-map 4 5, 6 7)))))
  "{1 {12345 123456},\n 3 {4 5, 6 7}}\n"

  #_(binding [cljs.pprint/*print-right-margin* 8]
    (with-out-str (pprint (sorted-set 123 456 789))))
  #_"#{123\n  456\n  789}\n"
  )

