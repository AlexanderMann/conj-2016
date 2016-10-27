(ns conj-2016.util.matrix-text
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators :as tc.gen]
            [clojure.test.check.properties :as tc.prop]
            [conj-2016.util.matrix :refer :all]
            [conj-2016.util.comparator :as u.comp]
            [conj-2016.util.generator :as t.u.gen]
            [incanter.core :as i.core]))


(tc.ct/defspec
  random-matrix-is-of-expected-size
  100
  (tc.prop/for-all
    [num-rows tc.gen/s-pos-int
     num-cols tc.gen/s-pos-int]
    (u.comp/dimensions-match? (rand-matrix num-rows num-cols)
                              num-rows
                              num-cols)))

(tc.ct/defspec
  one-matrix-is-of-expected-size
  100
  (tc.prop/for-all
    [num-rows tc.gen/s-pos-int
     num-cols tc.gen/s-pos-int]
    (u.comp/dimensions-match? (one-matrix num-rows num-cols)
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
    [g-matrix (t.u.gen/gen-matrix* (tc.gen/double*
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
    (u.comp/fuzzy= g-matrix
                   (log (exp g-matrix))
                   :num-tolerance 1)))

(tc.ct/defspec
  log->exp
  100
  (tc.prop/for-all
    [g-matrix t.u.gen/gen-s-pos-matrix]
    (u.comp/fuzzy= g-matrix
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
                 (t.u.gen/gen-matrix* (tc.gen/bind
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
          (and (u.comp/fuzzy= mutable-matrix original-matrix)
               (update!-non-diag mutable-matrix row-n update-vec)
               (not (u.comp/fuzzy= mutable-matrix original-matrix))
               (u.comp/fuzzy= (m/get-row mutable-matrix row-n)
                              (m/mset update-vec row-n (m/mget original-matrix row-n row-n))))]
      result)))
