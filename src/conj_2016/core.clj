(ns conj-2016.core
  (:require [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.string :as string]
            [conj-2016.tsne :as tsne]
            [conj-2016.util.graph :as graph]
            [dali ]
            [incanter.core :as i.core])
  (:import [java.util.zip GZIPInputStream]))

(def dimensions 2)

(defn gunzip
  [path]
  (with-open [in (GZIPInputStream.
                  (clojure.java.io/input-stream
                   path))]
    (slurp in)))

(defn parse-embeddings
  [path]
  (let [unzipped (gunzip path)
        lines (string/split-lines unzipped)
        line-parser (fn [v l]
                      (let [[c & nums] (string/split l #" ")]
                        (assert (not (contains? v c))
                                (format "%s already in parsed embeddings. Duplicates are not allowed"
                                        c))
                        (assert (or (empty? v)
                                    (= (-> v vals first count)
                                       (count nums)))
                                (format "Width of %d is different from required width of %d for %s"
                                        (count nums) (-> v vals first count) c))
                        (assoc v
                          c (map read-string nums))))
        vecs (vec (reduce line-parser {} lines))]
    [(map first vecs)
     (i.core/matrix (map second vecs))]))

(def inp (parse-embeddings (io/resource "english-embeddings.turian.txt.gz")))

(defn init-matrix
  "Given the output of parse-embeddings, initialize a matrix. Output is an array, where
  the first element is a list of strings and the second is a matrix. The list of strings
  correlate to the rows in the matrix, where each string is a key from parsed-embeddings"
  [parsed-embeddings]
  (let [vecs (vec parsed-embeddings)
        coords (tsne/tsne (i.core/matrix (map second vecs))
                     dimensions
                     30.0)]
    #_(graph/page (m/to-nested-vectors coords)
                (map first vecs))
    coords))

(comment
  (def inp (parse-embeddings (io/resource "english-embeddings.turian.txt.gz")))

  (def tsne-inp (init-matrix inp))
  (first tsne-inp)
  (m/to-nested-vectors tsne-inp))

;(m/maximum (last (init-matrix inp)))
