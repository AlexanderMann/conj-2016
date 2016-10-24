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

(defn- range-intersect?
  [[r0 r1] [r2 r3] num-tolerance]
  ;(println (<= r0 r3) (<= r2 r1) (< (Math/abs (- r2 r1)) num-tolerance) r0 r1 r2 r3 num-tolerance)
  (and (<= r0 r3)
       (or (<= r2 r1)
           (< (Math/abs (- r2 r1)) num-tolerance))))

(defn- fuzzy=
  "Diff doesn't do a great job at helping us with numbers that are realllllyyy close.
  When sending values through the shell to Python and diffing them against Matrix
  multiplication values in the JVM...numerical percision becomes signicant."
  [a b & {:keys [num-tolerance
                 relative?
                 bracket-by-ulp?
                 ulp-modifier]
          :or {num-tolerance 0
               relative? false
               bracket-by-ulp? false
               ulp-modifier identity}
          :as opts}]
  (cond
    (= a b) true

    (and (non-numeric-number? a)
         (non-numeric-number? b))
    true

    (and (number? a)
         (number? b))
    (or (and bracket-by-ulp?
             (let [a-ulp (ulp-modifier (Math/ulp a))
                   b-ulp (ulp-modifier (Math/ulp b))
                   a-range [(- a a-ulp) (+ a a-ulp)]
                   b-range [(- b b-ulp) (+ b b-ulp)]]
               (or (range-intersect? a-range b-range num-tolerance)
                   (range-intersect? b-range a-range num-tolerance))))
        (and (let [d (Math/abs (- a b))]
               (if relative?
                 (< (->> (min a b)
                         Math/abs
                         (max Double/MIN_VALUE)
                         (/ d))
                    num-tolerance)
                 (< d num-tolerance)))))

    (and (seqable? a)
         (not (map? a))
         (seqable? b)
         (not (map? b)))
    (every? identity
            (map #(fuzzy= %1 %2
                          :num-tolerance num-tolerance
                          :relative? relative?
                          :bracket-by-ulp? bracket-by-ulp?
                          :ulp-modifier ulp-modifier)
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
    [g-matrix (gen-matrix* (tc.gen/double*
                             {:infinite? false
                              :NaN? false
                              :min (->> (for [i (range 0 1000 0.01)]
                                          [(* -1 i) (Math/exp (double (* -1 i)))])
                                        (remove #(zero? (last %)))
                                        last
                                        first)
                              :max (->> (for [i (range 0 1000 0.01)]
                                          [i (Math/exp (double i))])
                                        (remove #(.isInfinite (last %)))
                                        last
                                        first)}))]
    (fuzzy= g-matrix
            (log (exp g-matrix))
            :num-tolerance 1)))

(tc.ct/defspec
  log->exp
  100
  (tc.prop/for-all
    [g-matrix gen-s-pos-matrix]
    (fuzzy= g-matrix
            (exp (log g-matrix))
            :num-tolerance 1
            :relative? true)))

(tc.ct/defspec
  matrix->update!-non-diag_single-element
  100
  (tc.prop/for-all
    [g-payload (tc.gen/bind
                 tc.gen/double
                 (fn [v]
                   (tc.gen/tuple
                     (tc.gen/return (i.core/matrix [[v]]))
                     (tc.gen/fmap
                       #(i.core/matrix [%])
                       (tc.gen/such-that
                         (partial not= v)
                         tc.gen/double)))))]
    (let [[target-matrix
           update-vec] g-payload
          original-matrix target-matrix]
      (and (= original-matrix target-matrix)
           (update!-non-diag target-matrix 0 update-vec)
           (= original-matrix target-matrix)
           (not= (m/mget target-matrix 0 0)
                 (m/mget update-vec 0))))))

(tc.ct/defspec
  matrix->update!-non-diag_strictly-non-single-elements
  100
  (tc.prop/for-all
    [g-payload (tc.gen/bind
                 (gen-matrix* (tc.gen/bind
                                tc.gen/s-pos-int
                                (fn [n] (tc.gen/tuple
                                          (tc.gen/return (inc n))
                                          (tc.gen/return (inc n)))))
                              tc.gen/double)
                 (fn [matrix_n_n]
                   (let [n (m/row-count matrix_n_n)]
                     (tc.gen/bind
                       (tc.gen/choose 0 (dec n))
                       (fn [i]
                         (tc.gen/tuple
                           (tc.gen/return matrix_n_n)
                           (tc.gen/return i)
                           (tc.gen/fmap
                             i.core/matrix
                             (tc.gen/vector
                               (tc.gen/such-that
                                 #(as-> matrix_n_n $
                                        (m/get-row $ i)
                                        (into #{} $)
                                        (contains? $ %)
                                        (not $))
                                 tc.gen/double)
                               n))))))))]
    (let [[original-matrix
           row-n
           update-vec] g-payload
          mutable-matrix (m/clone original-matrix)
          result
          (and (fuzzy= mutable-matrix original-matrix)
               (update!-non-diag mutable-matrix row-n update-vec)
               (not (fuzzy= mutable-matrix original-matrix))
               (fuzzy= (m/get-row mutable-matrix row-n)
                       (m/mset update-vec row-n (m/mget original-matrix row-n row-n))))]
      result)))

(defn- diff-hbeta
  [g-matrix g-beta]
  (let [{:keys [data success?] :as expected-hbeta} (u.py/python-result "textSNE" "tsne" "Hbeta"
                                                              g-matrix g-beta)
        actual-hbeta (map m/to-nested-vectors (hbeta g-matrix g-beta))]
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
  hbeta-matches-pyton-impl
  100
  prop-hbeta)

(comment
  (tc/quick-check 10 prop-matrix->update!-non-giag)

  (let [m (i.core/matrix [[1]])
        v (i.core/matrix [32])]
    (println (update!-non-diag m 1 v))
    (println m))

  (diff-hbeta [[1.0]] 1.0)
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
    (map p r))
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
    (and success?
         (fuzzy= data
                 actual-hbeta
                 :num-tolerance 0.01
                 :relative? true)))

  (run-tests))
