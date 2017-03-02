(defproject conj-2016 "0.1.0-SNAPSHOT"
  :description "Repo for work pertaining to TSNE, Conj, and English Embeddings"
  :url "https://github.com/AlexanderMann/conj-2016"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :resource-paths ["resources"]
  :dependencies [[cheshire "5.7.0"]
                 ;[clatrix "0.5.0"]
                 [com.taoensso/timbre "4.8.0"]
                 [com.taoensso/tufte "1.1.1"]
                 ;[dali "0.7.3"]
                 ;[incanter "1.9.1"]
                 [net.mikera/core.matrix "0.57.0"]
                 [org.clojure/clojure "1.9.0-alpha14"]
                 [org.clojure/core.memoize "0.5.9"]
                 [org.clojure/data.csv "0.1.3"]
                 [org.clojure/test.check "0.9.0"]]
  :jvm-opts ["-Xmx8g"]
  :aliases {"nrepl" ["repl" ":headless" ":port" "54321"]})
