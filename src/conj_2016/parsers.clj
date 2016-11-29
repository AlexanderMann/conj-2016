(ns conj-2016.parsers
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.string :as string]
            [incanter.core :as i.core])
  (:import [java.util.zip GZIPInputStream]))

(defn gunzip
  [path]
  (with-open [in (GZIPInputStream.
                   (io/input-stream path))]
    (slurp in)))

(defn turian-demo-english-embeddings*
  []
  (let [lines (-> (io/resource "english-embeddings.turian.txt.gz")
                  gunzip
                  (csv/read-csv :separator \ ))
        line-parser (fn [v [label & nums]]
                      (assoc v
                        label (map #(Double/parseDouble %) nums)))
        vecs (vec (reduce line-parser {} lines))]
    [(map first vecs)
     (i.core/matrix (map second vecs))]))

(def turian-demo-english-embeddings (memoize turian-demo-english-embeddings*))

(defn mnist-embeddings*
  []
  (let [labels (->> (io/resource "pythonLibs/tsne_python/mnist2500_labels.txt")
                    slurp
                    csv/read-csv
                    (map (comp int
                               #(Double/parseDouble %)
                               string/trim
                               first)))
        data-lines (-> (io/resource "pythonLibs/tsne_python/mnist2500_X.txt")
                       slurp
                       (csv/read-csv :separator \ ))
        data-parser (fn [nums]
                      (->> nums
                           (remove empty?)
                           (map #(Double/parseDouble %))))
        data (map data-parser data-lines)]
    [labels
     (i.core/matrix data)]))

(def mnist-embeddings (memoize mnist-embeddings*))
