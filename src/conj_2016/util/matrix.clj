(ns conj-2016.util.matrix
  (:require [incanter.core :as i.core]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.random :as m.rnd])
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

(defn conj-idx
  [^AVector avector val idx]
  (m/join (i.core/matrix []) (take idx avector) [val] (drop idx avector)))

(defn m-every?
  [pred matrix]
  (every? pred (m/as-vector matrix)))
