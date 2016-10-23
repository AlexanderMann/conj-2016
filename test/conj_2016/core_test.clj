(ns conj-2016.core-test
  (:require [clojure.core.matrix :as m]
            [clojure.data :as d]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators :as tc.gen]
            [clojure.test.check.properties :as tc.prop]
            [conj-2016.core :refer :all]
            [conj-2016.util.python :as u.py]
            [incanter.core :as i.core])
  (:import [mikera.matrixx Matrix]))

(defn- gen-matrix*
  [num-gen]
  (tc.gen/bind
    tc.gen/s-pos-int
    (fn [n]
      (tc.gen/not-empty
        (tc.gen/vector
          (tc.gen/not-empty
            (tc.gen/vector num-gen
                           n)))))))

(def gen-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false})))

(def gen-s-pos-matrix
  (gen-matrix* (tc.gen/double* {:infinite? false :NaN? false :min Double/MIN_VALUE})))

(defn dimensions-match?
  [matrix num-rows num-cols]
  (and (= num-rows (m/row-count matrix))
       (= num-cols (m/column-count matrix))))

(defn non-numeric-number?
  "Returns true when nil, or is a double which is NaN/+-Inf"
  [obj]
  (when (or (nil? obj)
            (double? obj))
    (or (nil? obj)
        (.isNaN obj)
        (.isInfinite obj))))

(defn- fuzzy=
  "Diff doesn't do a great job at helping us with numbers that are realllllyyy close.
  When sending values through the shell to Python and diffing them against Matrix
  multiplication values in the JVM...numerical percision becomes signicant."
  [a b & {:keys [num-tolerance
                 relative?]
          :or {num-tolerance 0
               relative? false}
          :as opts}]
  (cond
    (= a b) true

    (and (non-numeric-number? a)
         (non-numeric-number? b))
    true

    (and (number? a)
         (number? b))
    (let [d (Math/abs (- a b))]
      ;(println d a b)
      (if relative?
        (< (->> (min a b)
                Math/abs
                (max Double/MIN_VALUE)
                (/ d))
           num-tolerance)
        (< d num-tolerance)))

    (and (seqable? a)
         (not (map? a))
         (seqable? b)
         (not (map? b)))
    (every? identity
            (map #(fuzzy= %1 %2
                          :num-tolerance num-tolerance
                          :relative? relative?)
                 a
                 b))

    :else (throw (ex-info "This data type hasn't been implemented!!!"
                          {:a a
                           :b b
                           :opts opts}))))

(tc.ct/defspec
  random-matrix-is-of-expected-size
  100
  (tc.prop/for-all
    [num-rows tc.gen/s-pos-int
     num-cols tc.gen/s-pos-int]
    (dimensions-match? (rand-matrix num-rows num-cols)
                       num-rows
                       num-cols)))

(tc.ct/defspec
  one-matrix-is-of-expected-size
  100
  (tc.prop/for-all
    [num-rows tc.gen/s-pos-int
     num-cols tc.gen/s-pos-int]
    (dimensions-match? (one-matrix num-rows num-cols)
                       num-rows
                       num-cols)))

(tc.ct/defspec
  one-matrix-is-all-ones
  100
  (tc.prop/for-all
    [num-rows tc.gen/s-pos-int
     num-cols tc.gen/s-pos-int]
    (every? (partial = 1.0)
            (m/eseq (one-matrix num-rows num-cols)))))

(tc.ct/defspec
  exp->log
  100
  (tc.prop/for-all
    [g-matrix gen-matrix]
    (let [matrix (i.core/matrix g-matrix)]
      (fuzzy= matrix
              (log (exp matrix))
              :num-tolerance 0.0000001))))

(tc.ct/defspec
  log->exp
  100
  (tc.prop/for-all
    [g-matrix gen-s-pos-matrix]
    (let [matrix (i.core/matrix g-matrix)]
      (fuzzy= matrix
              (exp (log matrix))
              :num-tolerance 0.0000001))))

(defn- diff-hbeta-calc-p
  [g-matrix g-beta]
  (let [matrix (i.core/matrix g-matrix)
        o1 (u.py/shell-parseable matrix)
        o2 (u.py/shell-parseable g-beta)

        {:keys [data success?] :as expected-hbeta}
        (u.py/python-inline-result
          "textSNE" "tsne"
          (format "numpy.exp(-(%s).copy() * %s)"
                  (:o o1)
                  (:o o2))
          (concat (:deps o1) (:deps o2)))

        actual-hbeta (map m/to-nested-vectors (hbeta matrix g-beta))]
    ;(println expected-hbeta)
    ;(println data)
    ;(println actual-hbeta)
    ;(def snagged [data actual-hbeta])
    (and success?
         (fuzzy= data
                 actual-hbeta
                 :num-tolerance 0.01
                 :relative? true))))

(defn- diff-hbeta
  [g-matrix g-beta]
  (let [matrix (i.core/matrix g-matrix)
        {:keys [data success?] :as expected-hbeta} (u.py/python-result "textSNE" "tsne" "Hbeta"
                                                              matrix g-beta)
        actual-hbeta (map m/to-nested-vectors (hbeta matrix g-beta))]
    ;(println expected-hbeta)
    ;(println data)
    ;(println actual-hbeta)
    ;(def snagged [data actual-hbeta])
    (and success?
         (fuzzy= data
                 actual-hbeta
                 :num-tolerance 0.001
                 :relative? true))))

(def prop-hbeta
  (tc.prop/for-all
    [matrix gen-matrix
     beta (tc.gen/double* {:infinite? false :NaN? false :min 1.0})]
    (diff-hbeta matrix beta)))

(tc.ct/defspec
  hbeta-matches-original
  100
  prop-hbeta)

(comment
  (tc/quick-check 100 prop-hbeta)

  (diff-hbeta [[1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 -18.916748046875 1.0]] 4.0)
  (class (m/matrix [[1.0M 2.0M][3.0M 4.0M]]))
  (def r
    (for [x (range 5)
          y (range 5)
          i (range x)]
      (let [matrix (rand-matrix x y)
            o1 (u.py/shell-parseable matrix)
            o2 (u.py/shell-parseable i)
            o3 (u.py/shell-parseable (m/row-count matrix))]
        [[x y i matrix]
         (u.py/python-inline-result
           "textSNE" "tsne"
           (format "%1$s[%2$s, numpy.concatenate((numpy.r_[0:%2$s], numpy.r_[%2$s+1:%3$s]))]"
                   (:o o1)
                   (:o o2)
                   (:o o3))
           (concat (:deps o1) (:deps o2) (:deps o3)))])))

  (let [p (fn [[[x y i matrix] {:keys [success? data]}]]
            [x y i matrix success? data])]
    (map p r)))



;Math.exp(-D.copy() * beta)

;(run-tests)
