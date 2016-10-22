(ns conj-2016.core-test
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.clojure-test :as tc.ct]
            [clojure.test.check.generators :as tc.gen]
            [clojure.test.check.properties :as tc.prop]
            [conj-2016.core :refer :all]
            [incanter.core :as i.core])
  (:import [mikera.matrixx Matrix]))

(defn dimensions-match?
  [matrix num-rows num-cols]
  (and (= num-rows (m/row-count matrix))
       (= num-cols (m/column-count matrix))))

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



(run-tests)