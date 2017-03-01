(ns conj-2016.parsers
  "Used to take disparate sources for 'notable' words and parse them
  into sets of words used to filter down various data language model
  data sets."
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

(def hiiamrohit-base-url "https://raw.githubusercontent.com/hiiamrohit/Countries-States-Cities-database/master/")
(def hiiamrohit-countries-url (str hiiamrohit-base-url "countries.sql"))
(def hiiamrohit-states-url (str hiiamrohit-base-url "states.sql"))
(def hiiamrohit-cities-url (str hiiamrohit-base-url "cities.sql"))

(defn- places-countries*
  []
  (->> hiiamrohit-countries-url
       slurp
       string/split-lines
       (map (partial re-matches #"\(\d+, '(\w+?)', '(\w+?)'\).*"))
       (remove nil?)
       (map rest)
       flatten))

(defn- places-states*
  []
  (->> hiiamrohit-states-url
       slurp
       string/split-lines
       (map (partial re-matches #"\(\d+, '(.+?)',.*"))
       (remove nil?)
       (map second)))

(defn- places-cities*
  []
  (->> hiiamrohit-cities-url
       slurp
       string/split-lines
       (map (partial re-matches #"\(\d+, '(.+?)',.*"))
       (remove nil?)
       (map second)))

(def places-countries (memoize places-countries*))
(def places-states (memoize places-states*))
(def places-cities (memoize places-cities*))

(defn places-words
  []
  (->> (concat
         (places-countries)
         (places-states)
         (places-cities))
       (map string/lower-case)
       (into #{})))

(defn most-common-words
  []
  (->> (io/resource "3000-most-common-english-words.txt")
       slurp
       string/split-lines
       (map string/lower-case)
       (into #{})))

(defn notable-words
  []
  (-> (places-words)
      (into (most-common-words))
      (into (map str (range 0 10)))
      (into (->> (range 0 129)
                 (map char)
                 (map str)))))

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

(defn collobert-weston-embeddings*
  []
  (let [labels (with-open [rdr (clojure.java.io/reader (io/resource "senna-embeddings/words.lst"))]
                 (doall (line-seq rdr)))
        data-lines (-> (io/resource "senna-embeddings/embeddings.txt")
                       slurp
                       (csv/read-csv :separator \ ))
        data-parser (fn [nums]
                      (map #(Double/parseDouble %) nums))
        data (map data-parser data-lines)]
    [labels
     (i.core/matrix data)]))

(def collobert-weston-embeddings (memoize collobert-weston-embeddings*))

(defn collobert-weston-embeddings-filtered
  [notable-set]
  (let [zipmapped (apply zipmap (collobert-weston-embeddings))
        is-notable? (fn [[label data]]
                      (notable-set (string/lower-case label)))
        notable-embeddings (filter is-notable? zipmapped)]
    [(map first notable-embeddings)
     (i.core/matrix (map second notable-embeddings))]))

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
