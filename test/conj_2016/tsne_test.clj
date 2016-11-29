(ns conj-2016.tsne-test
  (:require [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators :as tc.gen]
            [clojure.test.check.properties :as tc.prop]
            [conj-2016.core :as core]
            [conj-2016.tsne :refer :all]
            [conj-2016.util.comparator :as u.comp]
            [conj-2016.util.generator :as t.u.gen]
            [conj-2016.util.python :as u.py]
            [incanter.core :as i.core]
            [taoensso.timbre :as log]
            [conj-2016.util.matrix :as u.m]))

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
        (log/error (format "\nFAILED: %s\nEXPECTED:\n%s\n\nACTUAL:\n%s\nINPUT:\n%s\n"
                           py-call
                           (with-out-str
                             (pprint/pprint (if success?
                                              data
                                              expected)))
                           (with-out-str
                             (pprint/pprint actual))
                           args))
        (swap! python-impl-diff-recent-failures assoc py-call args))
      outcome)
    (catch Exception e
      (log/error e py-call))))

(defn diff-hbeta
  [g-vector g-beta]
  (python-impl-diff
    hbeta
    {:repo   "tsne_python"
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
    {:repo   "tsne_python"
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

(deftest x2p-diff-matches
  (is (diff-x2p (i.core/matrix
                  [[-0.413719972411,-2.33531369563,-4.36090896749,0.494096203118,-1.75734188309,-0.11629916381,-4.58464606884,3.57617595074,2.76392902999,4.0221554207,-0.881399240962,-0.222821200187,1.57708188454,4.02024202382,-5.93811953025,-0.958464874662,-0.570821227132,-2.95661262686,-5.87523845429,12.287955544,10.1883441072,3.08639755161,-1.98741318329,8.37506758507,1.27779775606,0.419222030935,-0.407476198897,1.59513276701,1.05286326445,-1.07650713746,-5.28805736076,-0.103139112862,-2.58077656305,2.63089190121,-0.957356765985,-2.55498294743,-3.19707038254,-9.37859895772,2.58658606677,2.76856337907,-0.464005333616,-3.44977989437,3.3509508361,4.77203951177,-7.26506309773,3.88772850464,0.0542195997435,4.03575674779,-0.326684501457,2.81217860905],
                   [2.17604433435,0.262062592529,0.119701283255,-1.75273187736,-1.77461157806,7.35459478175,2.35314491584,-1.22647892435,-1.10831330656,4.23167101727,-2.72226332095,-1.70400234086,-2.86433215862,-2.81186901757,-2.30290222581,-3.10796910447,0.957122975708,6.04421462027,-0.253799337248,5.49724562276,5.22384253878,0.451308188685,4.02673951821,5.42703918269,-4.95297167283,-9.02093961276,4.56421144738,0.979508693252,-1.02148853609,-3.67380804931,1.08843462869,4.99976896608,-3.04748337382,1.74916249917,0.938733784348,-3.84605255792,-5.17646377953,-1.04623865964,2.37913385568,10.2080891846,2.38845120872,-5.77781393646,-6.59270914966,-3.61119567371,-1.8965798314,0.257938448285,3.49546180974,-0.821529728449,3.88022729138,2.31385928421],
                   [-4.29968538127,-1.55273441301,-4.06791888988,3.10421779713,-6.6835610841,6.83492494534,-1.37624299032,7.92628887256,5.45202565101,4.46669926835,-0.552670993268,-4.92693654058,-5.73289885308,5.98632701975,-5.86737841364,-1.51057735704,-1.53001095269,1.01418355479,-1.28014411313,10.5499311887,3.33989358771,4.47184244115,2.20671676266,4.53051212792,-6.85708715164,3.61666018514,4.38618599463,-3.27225471554,-0.162379353801,-1.00174933918,-3.04022881784,-2.88708088112,0.0935644146862,2.02098975996,4.28899400482,-0.545051149281,-6.52296454117,-3.55818045399,0.905592010068,9.62788875943,-1.51864363779,-0.412800155162,-4.88788778101,3.52999976021,-2.58367657194,1.5244271785,-3.00383628601,1.78359446474,0.590308775483,2.76214721487]])
                1E-5 30.0)))

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
                 (i.core/matrix [[1.25 -1.0] [-2.0 1.75] [-1.5 1.0]])))
  #_(testing "tsne input"
    (is (diff-tsne (m/select (second (core/parse-embeddings (io/resource "english-embeddings.turian.txt.gz")))
                             (range 3) :all)
                   10.0
                   (u.m/rand-matrix 3 2)))))

;(tsne-diff-matches)
