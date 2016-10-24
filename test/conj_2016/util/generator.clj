(ns conj-2016.util.generator
  (:require [clojure.test.check.generators :as tc.gen]
            [incanter.core :as i.core]))

(defn gen-matrix*
  ([value-generator]
   (gen-matrix* (tc.gen/tuple tc.gen/s-pos-int tc.gen/s-pos-int)
                value-generator))
  ([dimension-generator value-generator]
   (tc.gen/fmap
     i.core/matrix
     (tc.gen/bind
       dimension-generator
       (fn [[n m]]
         (tc.gen/vector
           (tc.gen/vector value-generator
                          m)
           n))))))

(def gen-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false})))

(def gen-s-pos-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false :min Double/MIN_VALUE})))

(def gen-square-matrix
  (gen-matrix* (tc.gen/bind
                 tc.gen/s-pos-int
                 (fn [n] (tc.gen/tuple (tc.gen/return n) (tc.gen/return n))))
               (tc.gen/double* {:infinite? false :NaN? false})))
