(ns clj.contrib.pprint-test
  (:require
    [cemerick.cljs.test :refer [deftest is]]))

(defmacro simple-tests [name & test-pairs]
  `(deftest ~name
     ~@(for [[x y] (partition 2 test-pairs)]
         `(cljs.core/cond
            (cljs.core/= js/RegExp (cljs.core/type ~y)) (is (.exec ~y ~x))
            (cljs.core/= js/String (cljs.core/type ~y)) (is (cljs.core/= ~x ~y))
            :else (is (cljs.core/= ~x ~y))))))