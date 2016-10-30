(ns conj-2016.core-test
  (:require [clojure.core.matrix :as m]
            [clojure.data :as d]
            [clojure.pprint :as pprint]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators :as tc.gen]
            [clojure.test.check.properties :as tc.prop]
            [conj-2016.core :refer :all]
            [conj-2016.util.comparator :as u.comp]
            [conj-2016.util.generator :as t.u.gen]
            [conj-2016.util.python :as u.py]
            [incanter.core :as i.core]
            [taoensso.timbre :as log]))

(def num-tests 50)

(def python-impl-diff-recent-failures (atom {}))

(defn python-impl-diff
  [clojure-impl {:keys [repo module fname] :as py-call} fuzzy=-opts & args]
  (try
    (let [{:keys [data success?] :as expected} (apply u.py/python-result repo module fname
                                                      args)
          actual (map m/to-nested-vectors (apply clojure-impl (map m/clone args)))
          outcome (and success?
                       (apply u.comp/fuzzy=
                              data actual
                              (-> fuzzy=-opts vec flatten)))]
      (when-not outcome
        (swap! python-impl-diff-recent-failures assoc py-call args)
        (when-not (get @python-impl-diff-recent-failures py-call)
          (log/error (format "\nFAILED: %s\nEXPECTED:\n%s\n\nACTUAL:\n%s\nINPUT:\n%s\n"
                             py-call
                             (with-out-str
                               (pprint/pprint (if success?
                                                data
                                                expected)))
                             (with-out-str
                               (pprint/pprint actual))
                             args))))
      outcome)
    (catch Exception e
      (log/error e py-call))))

(defn diff-hbeta
  [g-vector g-beta]
  (python-impl-diff
    hbeta
    {:repo   "textSNE"
     :module "tsne"
     :fname  "Hbeta"}
    {:num-tolerance 0.001}
    g-vector g-beta))

(def hbeta-prop
  (tc.prop/for-all
    [g-vector t.u.gen/gen-avector
     g-beta (t.u.gen/double* {:infinite? false :NaN? false :min 1.0 :max 50 :precision 5})]
    (diff-hbeta g-vector g-beta)))

(deftest hbeta-testing
  (testing "diff matches"
    (is (diff-hbeta (i.core/matrix [0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0])
                    2.0))
    (is (diff-hbeta (i.core/matrix [0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0])
                    Double/POSITIVE_INFINITY))
    (is (diff-hbeta (i.core/matrix [0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.0])
                    7.0))))

(tc.ct/defspec
  hbeta-matches-python-impl
  num-tests
  hbeta-prop)

(defn diff-x2p
  [g-matrix g-tolerance g-perplexity]
  (python-impl-diff
    x2p
    {:repo   "textSNE"
     :module "tsne"
     :fname  "x2p"}
    {:num-tolerance 0.001}
    g-matrix g-tolerance g-perplexity))

(def x2p-prop
  (tc.prop/for-all
    [g-matrix t.u.gen/gen-matrix
     g-tolerance (t.u.gen/double* {:infinite? false :NaN? false :min Double/MIN_VALUE :max 0.1 :precision 5})
     g-perplexity (tc.gen/double* {:infinite? false :NaN? false :min 10.0 :max 100.0 :precision 3})]
    (diff-x2p g-matrix g-tolerance g-perplexity)))

(tc.ct/defspec
  x2p-matches-python-impl
  num-tests
  x2p-prop)

(defn diff-gradient
  [g-matrix_n_m g-pvalues_n_n]
  (python-impl-diff
    gradient
    {:repo   "textSNE"
     :module "tsne"
     :fname  "gradient"}
    {:num-tolerance 0.001}
    g-matrix_n_m g-pvalues_n_n))

(def gradient-prop
  (tc.prop/for-all
    [g-matrix-payload (tc.gen/bind
                        (tc.gen/choose 5 100)
                        (fn [n]
                          (tc.gen/tuple
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return 2))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -50.0 :max 50.0
                                                                   :precision 5})
                                                 true)
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return n))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -10.0 :max 10.0
                                                                   :precision 5})
                                                 true))))]
    (diff-gradient (first g-matrix-payload) (last g-matrix-payload))))

(tc.ct/defspec
  gradient-matches-python-impl
  num-tests
  gradient-prop)

(defn diff-gains
  [g-gains_n_m g-gradient_n_m g-intermediate_n_m]
  (python-impl-diff
    update!-gains
    {:repo   "textSNE"
     :module "tsne"
     :fname  "update_gains"}
    {:num-tolerance 0.001}
    g-gains_n_m g-gradient_n_m g-intermediate_n_m))

(def gains-prop
  (tc.prop/for-all
    [g-matrix-payload (tc.gen/bind
                        (tc.gen/choose 5 100)
                        (fn [n]
                          (tc.gen/tuple
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return 2))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -50.0 :max 50.0
                                                                   :precision 5})
                                                 true)
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return 2))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -50.0 :max 50.0
                                                                   :precision 5})
                                                 true)
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return 2))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -50.0 :max 50.0
                                                                   :precision 5})
                                                 true))))]
    (diff-gains (first g-matrix-payload) (second g-matrix-payload) (last g-matrix-payload))))

(tc.ct/defspec
  gains-matches-python-impl
  num-tests
  gains-prop)

(defn diff-tsne
  [g-matrix_n_m g-perplexity g-rand-matrix_n_2]
  (python-impl-diff
    tsne
    {:repo   "textSNE"
     :module "tsne"
     :fname  "tsne"}
    {:num-tolerance 1}
    g-matrix_n_m 2 nil g-perplexity false g-rand-matrix_n_2))

(def tsne-prop
  (tc.prop/for-all
    [g-matrix-payload (tc.gen/bind
                        (tc.gen/choose 5 100)
                        (fn [n]
                          (tc.gen/tuple
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/choose 3 500))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -50.0 :max 50.0
                                                                   :precision 5})
                                                 true)
                            (t.u.gen/gen-matrix* (tc.gen/tuple (tc.gen/return n)
                                                               (tc.gen/return 2))
                                                 (t.u.gen/double* {:infinite? false :NaN? false :min -10.0 :max 10.0
                                                                   :precision 5})
                                                 true))))
     g-perplexity (t.u.gen/double* {:infinite? false :NaN? false :min 10.0 :max 100.0
                                    :precision 3})]
    (diff-tsne (first g-matrix-payload) g-perplexity (last g-matrix-payload))))

(tc.ct/defspec
  tsne-matches-python-impl
  num-tests
  tsne-prop)

(deftest tsne-diff-matches
  (is (diff-tsne (i.core/matrix [[1.125 1.75 1.625]
                                 [-2.0 0.5 2.0]
                                 [-1.0 0.75 -0.5]])
                 10.0
                 (i.core/matrix [[1.25 -1.0] [-2.0 1.75] [-1.5 1.0]]))))

;(tsne-diff-matches)

(comment
  (tc/quick-check 10 tsne-prop)

  (diff-gradient (i.core/matrix [[1.25 3.0]
                                 [1.5 1.0]
                                 [-1.0 1.75]
                                 [2.0 1.125]
                                 [-1.5 0.5]])
                 (i.core/matrix [[0.0 -1.0 1.25 -2.0 1.5]
                                 [1.75 -0.5 1.125 1.625 3.5]
                                 [3.0 0.75 1.375 1.875 1.0625]
                                 [0.625 1.0 -1.75 -1.5 2.0]
                                 [2.5 1.5625 0.5 1.3125 -1.25]]))
  (diff-gains (i.core/matrix [[1.5 0.5] [1.25 1.75] [3.0 2.0] [-1.0 -2.0] [1.125 1.0]])
              (i.core/matrix [[1.25 1.0] [1.5 0.0] [0.5 3.0] [2.0 -1.0] [1.75 -0.5]])
              (i.core/matrix [[1.5 1.0] [-0.5 -1.0] [2.0 -0.0] [-2.0 0.5] [0.0 1.25]]))

  (log/error e {:os-arg-limit    os-arg-limit
                :args-in-command (-> python-bash
                                     (nth 2)
                                     (string/split #" ")
                                     count)
                :str-length      (count (str python-bash))})
  (def os-arg-limit (-> (shell/sh "getconf" "ARG_MAX")
                        :out
                        string/trim
                        Integer/parseInt
                        (+ 100)))

  (tc/quick-check 100 gains-prop)

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

        actual-hbeta (map m/to-nested-vectors (hbeta matçrix g-beta))]
    (and success?
         (fuzzy= data
                 actual-hbeta
                 :num-tolerance 0.01
                 :relative? true)))
  time

  {:failures [[1 2 3]]
   :python-timings [{:start 123 :end 456}]
   :clojure-timings [{:start 123 :end 456}]}

  (update-in {} [:k :v] (constantly 1))

  (run-tests))
