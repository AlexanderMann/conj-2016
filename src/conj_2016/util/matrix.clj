(ns conj-2016.util.matrix
  (:require [incanter.core :as i.core]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.random :as m.rnd]
            [taoensso.timbre :as log])
  (:import [mikera.vectorz AVector]
           [mikera.matrixx Matrix]))

(defn rand-matrix
  [num-rows num-cols]
  (i.core/matrix (repeatedly num-rows (partial m.rnd/sample-normal num-cols))))

(defn n-matrix
  [num-rows num-cols n]
  (m/add (m/zero-matrix num-rows num-cols) n))

(defn one-matrix
  [num-rows num-cols]
  (n-matrix num-rows num-cols 1))

(defn exp
  [^Matrix matrix]
  (m/emap (fn [^Double d]
            (Math/exp d))
          matrix))

(defn log
  [^Matrix matrix]
  (m/emap (fn [^Double d]
            (Math/log d))
          matrix))

(defn but-n
  "Given an AVector v, return a vector of all elements
  but the nth."
  [^AVector v n]
  (i.core/matrix (concat (take n v) (drop (inc n) v))))

(defn update!-non-diag
  "Updates the target row with the given vector expect for the diagonal element.

  NOTE: assumes correct dimensions for target_n_m and vector_1_m,
    also assumes that i is less than or equal to n and m.

  ex.
  [[1 2 3]
   [4 5 6]
   [0 0 9]]
  2
  [7 8 1000]
  ->
  [[1 2 3]
   [4 5 6]
   [7 8 9]]"
  [^Matrix target_n_m i ^AVector vector_1_m]
  (m/set-row! target_n_m i (m/mset vector_1_m i (m/mget target_n_m i i))))

(defn set!-diag
  "Set the diagonal value of target_n_n to be v"
  [^Matrix target_n_n v]
  (dotimes [idx (m/row-count target_n_n)]
    (m/mset! target_n_n idx idx v)))

(defn normalize!
  "Normalize the target matrix in place"
  [^Matrix target_n_m]
  (m/div! target_n_m (m/esum target_n_m)))

(defn clamp!
  ([^Matrix target_n_m min-val]
   (clamp! target_n_m min-val Double/POSITIVE_INFINITY))
  ([^Matrix target_n_m min-val max-val]
   (m/emap! #(max (min (double %)
                       (double max-val))
                  (double min-val))
            target_n_m)))

(defn conj-idx
  [^AVector avector val idx]
  (m/join (i.core/matrix []) (take idx avector) [val] (drop idx avector)))

(defn m-every?
  [pred matrix]
  (every? pred (m/as-vector matrix)))

(defn pairwise-distances
  "Compute the pairwise distances for a given n_m matrix."
  [^Matrix matrix_n_m]
  ;47 print "Computing pairwise distances..."
  ;49 sum_X = Math.sum(Math.square(X), 1);
  ;50 D = Math.add(Math.add(-2 * Math.dot(X, X.T), sum_X).T, sum_X);
  ; ...
  ;147 # Compute pairwise affinities
  ;148 sum_Y = Math.sum(Math.square(Y), 1);
  ;149 num = 1 / (1 + Math.add(Math.add(-2 * Math.dot(Y, Y.T), sum_Y).T, sum_Y));
  (let [sum-x_n (m/slice-map (comp m/esum m/square)
                             matrix_n_m)
        distances_n_n (m/add! (m/transpose!
                                (m/add! (m/mul! (m/mmul matrix_n_m
                                                        (m/transpose matrix_n_m))
                                                -2.0)
                                        sum-x_n))
                              sum-x_n)]
    distances_n_n))
