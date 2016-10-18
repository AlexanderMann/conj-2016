(ns conj-2016.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.util.zip GZIPInputStream]))

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
                                (format "%s already in parsed embeddings. Duplicates are not allowed" c))
                        (assoc v
                          c (map read-string nums))))]
    (reduce line-parser {} lines)))

(def inp (parse-embeddings (io/resource "english-embeddings.turian.txt.gz")))
