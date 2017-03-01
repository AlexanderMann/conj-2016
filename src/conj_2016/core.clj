(ns conj-2016.core
  (:require [clojure.java.io :as io]
            [conj-2016.tsne :as tsne]
            [conj-2016.parsers :as parsers]
            [conj-2016.util.graph :as graph]
            [taoensso.timbre :as log]
            [taoensso.tufte :as tufte]))

(tufte/add-handler!
  :logging-println
  "*"
  (fn [m]
    (let [{:keys [stats-str_ ?id ?data]} m
          stats-str (force stats-str_)]
      (log/debug
        (str
          (when ?id   (str "\nid: "   ?id))
          (when ?data (str "\ndata: " ?data))
          "\n" stats-str)))))

(def root-dir "/tmp/"
  ; use the following to spit everything to renderings and overwrite the examples
  #_(-> (java.io.File. "")
        .getAbsolutePath
        (str "/renderings/")))

(defn- spit-tsne-dims
  [pwd-edn-file-name [titles data] n-dims perplexity iterations]
  (when-not (.exists (io/as-file pwd-edn-file-name))
    (log/info "running tsne algorithm"
              :pwd-edn-file-name pwd-edn-file-name
              :n-dims n-dims
              :perplexity perplexity
              :iterations iterations)
    (spit pwd-edn-file-name
          (pr-str [(map vec
                        (tsne/tsne data
                                   n-dims
                                   perplexity
                                   iterations))
                   titles]))))

(defn- spit-tsne-n
  ([file-name [titles data] n-dims perplexity iterations]
    (spit-tsne-n file-name [titles data] n-dims perplexity iterations {}))
  ([file-name [titles data] n-dims perplexity iterations fill-options]
    (let [pwd-file-name (str root-dir file-name "_" n-dims)
          edn-file (str pwd-file-name ".edn")]
      (log/info "spitting tsne"
                :file-name file-name
                :pwd-file-name pwd-file-name
                :n-dims n-dims
                :perplexity perplexity
                :iterations iterations)
      (spit-tsne-dims edn-file [titles data] n-dims perplexity iterations)
      (let [[coords titles] (read-string (slurp edn-file))]
        (condp = n-dims
          2 (graph/graph-spit
              pwd-file-name "svg" coords titles fill-options)
          3 (graph/three-dimensional-graph-spit
              pwd-file-name "svg" coords titles))))))

(defn spit-tsne
  ([file-name [titles data] perplexity iterations]
   (spit-tsne file-name [titles data] perplexity iterations {}))
  ([file-name [titles data] perplexity iterations fill-options]
   (try
     (spit-tsne-n file-name [titles data] 2 perplexity iterations fill-options)
     (spit-tsne-n file-name [titles data] 3 perplexity iterations fill-options)
     (catch Exception e
       (log/error e)))))

(comment
  ; run as many or as few of these as you like to generate your own pretty pictures!
  (do (spit-tsne "tsne-data_places"
                 (parsers/collobert-weston-embeddings-filtered
                   (parsers/places-words))
                 30
                 100)

      (spit-tsne "tsne-data_common-words"
                 (parsers/collobert-weston-embeddings-filtered
                   (parsers/most-common-words))
                 30
                 100)

      (spit-tsne "tsne-data_full-cw"
                 (parsers/collobert-weston-embeddings-filtered
                   (parsers/notable-words))
                 30
                 100))

  (tufte/profile
    {:dynamic? true}
    (spit-tsne "tsne-mnist"
               (parsers/mnist-embeddings)
               30
               1000
               {0 :cyan
                1 :magenta
                2 :orange
                3 :red
                4 :blue
                5 :green
                6 :fuchsia
                7 :lightskyblue
                8 :lime
                9 :darkorchid})))
