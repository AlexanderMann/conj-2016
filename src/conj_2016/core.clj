(ns conj-2016.core
  (:require [clojure.core.matrix :as m]
            [conj-2016.tsne :as tsne]
            [conj-2016.parsers :as parsers]
            [conj-2016.util.graph :as graph]
            [incanter.core :as i.core]
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

(def dimensions 2)

(defn init-matrix
  [parsed-embeddings]
  (let [vecs (vec parsed-embeddings)
        coords (tsne/tsne (i.core/matrix (map second vecs))
                     dimensions
                     30.0)]
    #_(graph/page (m/to-nested-vectors coords)
                (map first vecs))
    coords))

(comment

  (def tsne-mnist (tufte/profile
                    {:dynamic? true}
                    (tsne/tsne (second (parsers/mnist-embeddings))
                               2
                               30)))
  (graph/svg-spit "/tmp/dali.mnist.svg"
                  tsne-mnist
                  (first (parsers/mnist-embeddings))
                  {0 :cyan
                   1 :magenta
                   2 :orange
                   3 :red
                   4 :blue
                   5 :green
                   6 :fuchsia
                   7 :lightskyblue
                   8 :lime
                   9 :darkorchid}))
