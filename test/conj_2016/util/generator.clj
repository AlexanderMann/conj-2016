(ns conj-2016.util.generator
  (:require [clojure.test.check.generators :as tc.gen]
            [conj-2016.util.python :as u.py]
            [incanter.core :as i.core])
  (:import [java.math MathContext]))

(defn gen-matrix*
  ([value-generator]
   (gen-matrix* (tc.gen/tuple tc.gen/s-pos-int tc.gen/s-pos-int)
                value-generator))
  ([dimension-generator value-generator]
   (gen-matrix* dimension-generator value-generator false))
  ([dimension-generator value-generator uniq?]
   (tc.gen/fmap
     i.core/matrix
     (tc.gen/bind
       dimension-generator
       (fn [[n m]]
         (if uniq?
           (tc.gen/fmap
             (partial partition m)
             (tc.gen/vector-distinct
               value-generator
               {:num-elements (* n m)}))
           (tc.gen/vector
             (tc.gen/vector value-generator
                            m)
             n)))))))

(defn gen-avector*
  [dimension-generator value-generator]
  (tc.gen/fmap
    i.core/matrix
    (tc.gen/bind
      dimension-generator
      (fn [n]
        (tc.gen/vector value-generator
                       n)))))

(def gen-avector
  (gen-avector* tc.gen/pos-int (tc.gen/double* {:infinite? false :NaN? false})))

(def gen-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false})))

(def gen-s-pos-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false :min Double/MIN_VALUE})))

(def gen-square-matrix
  (gen-matrix* (tc.gen/bind
                 tc.gen/s-pos-int
                 (fn [n] (tc.gen/tuple (tc.gen/return n) (tc.gen/return n))))
               (tc.gen/double* {:infinite? false :NaN? false})))

(defn- to-precision
  [precision ^Double d]
  (.doubleValue (BigDecimal. d (MathContext. precision))))

(defn double*
  [{:keys [precision] :as opts}]
  (tc.gen/fmap
    (partial to-precision precision)
    (tc.gen/double* opts)))

(defn such-that-python-limit
  [gen]
  (tc.gen/such-that
    (fn [& args]
      (not (apply u.py/will-spit? args)))
    gen))
