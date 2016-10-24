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
            [conj-2016.util.comparator :as u.comp]
            [conj-2016.util.generator :as t.u.gen]
            [incanter.core :as i.core])
  (:import [mikera.matrixx Matrix]))

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

(tc.ct/defspec
  hbeta-matches-pyton-impl
  100
  (tc.prop/for-all
    [g-matrix t.u.gen/gen-matrix
     g-beta (tc.gen/double* {:infinite? false :NaN? false :min 1.0})]
    (let [{:keys [data success?] :as expected-hbeta} (u.py/python-result "textSNE" "tsne" "Hbeta"
                                                                         g-matrix g-beta)
          actual-hbeta (map m/to-nested-vectors (hbeta g-matrix g-beta))]
      (and success?
           (u.comp/fuzzy= data
                          actual-hbeta
                          :num-tolerance 0.001
                          :relative? true)))))

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
