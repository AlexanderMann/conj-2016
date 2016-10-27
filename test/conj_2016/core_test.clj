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
            [incanter.core :as i.core]))

(defn diff-hbeta
  [g-vector g-beta]
  (let [{:keys [data success?] :as expected-hbeta} (u.py/python-result "textSNE" "tsne" "Hbeta"
                                                                       g-vector g-beta)
        actual-hbeta (map m/to-nested-vectors (hbeta g-vector g-beta))]
    ;(println expected-hbeta)
    (and success?
         (u.comp/fuzzy= data
                        actual-hbeta
                        :num-tolerance 0.001
                        :relative? true))))

(def hbeta-prop
  (tc.prop/for-all
    [g-vector t.u.gen/gen-avector
     g-beta (tc.gen/double* {:infinite? false :NaN? false :min 1.0})]
    (diff-hbeta g-vector g-beta)))

(deftest hbeta-diff-matches
  (is (diff-hbeta (i.core/matrix [0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0])
                  2.0))
  (is (diff-hbeta (i.core/matrix [0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0])
                  Double/POSITIVE_INFINITY))
  (is (diff-hbeta (i.core/matrix [0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.0])
                  7.0)))

(tc.ct/defspec
  hbeta-matches-python-impl
  100
  hbeta-prop)

(defn diff-x2p
  [g-matrix g-tolerance g-perplexity]
  (let [{:keys [data success?] :as expected-x2p} (u.py/python-result "textSNE" "tsne" "x2p"
                                                                     g-matrix g-tolerance g-perplexity)
        actual-x2p (map m/to-nested-vectors (x2p g-matrix g-tolerance g-perplexity))]
    ;(println expected-x2p)
    (and success?
         (u.comp/fuzzy= data
                        actual-x2p
                        :num-tolerance 0.001))))

(def x2p-prop
  (tc.prop/for-all
    [g-matrix t.u.gen/gen-matrix
     g-tolerance (tc.gen/double* {:infinite? false :NaN? false :min Double/MIN_VALUE :max 0.1})
     g-perplexity (tc.gen/double* {:infinite? false :NaN? false :min 10.0 :max 100.0})]
    (diff-x2p g-matrix g-tolerance g-perplexity)))

(tc.ct/defspec
  x2p-matches-python-impl
  100
  x2p-prop)

(comment
  (tc/quick-check 10 prop-matrix->update!-non-giag)

  (diff-x2p
    (i.core/matrix
      [[1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.0]
       [1.5]
       [1.5]])
    0.0625
    10.0)

  (last (apply clojure.data/diff args))
  [(map (partial clojure.string/join ",") (take-last 2 (first args)))
   (map (partial clojure.string/join ",") (take-last 2 (second args)))]

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
