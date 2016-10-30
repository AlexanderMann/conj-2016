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
