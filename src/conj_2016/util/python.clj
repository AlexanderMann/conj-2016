(ns conj-2016.util.python
  (:require [clojure.core.matrix :as m]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [incanter.core :as i.core])
  (:import [clojure.lang PersistentVector]
           [mikera.matrixx Matrix]))

(defmulti shell-parseable type)

(defn- seq-parser
  [seqable]
  (let [nested-objs (map shell-parseable seqable)]
    {:deps (->> nested-objs
                (map :deps)
                flatten
                (remove nil?)
                vec)
     :o (->> nested-objs
             (map :o)
             (string/join ", "))}))

(defmethod shell-parseable PersistentVector
  [obj]
  (update (seq-parser obj)
          :o (partial format "[%s]")))

(defmethod shell-parseable Matrix
  [obj]
  (let [{:keys [deps o]} (shell-parseable (m/to-nested-vectors obj))]
    {:deps (conj deps "import numpy")
     :o (format "numpy.array(%s)"
                o)}))

(defmethod shell-parseable String
  [obj]
  {:o (string/escape obj {\" "\\\"" \' "\\'"})})

(defmethod shell-parseable :default
  [obj]
  {:o (str obj)})

(defn build-python-payload
  [& args]
  (seq-parser args))

(defn install-robust-encoder
  []
  (format "sys.path.append('%s')"
          (.getParent (io/file (io/resource "jsonEncoder.py")))))

(defn json-dumps
  [result]
  (format "json.dumps(%s, cls=jsonEncoder.RobustEncoder, separators=(',',':'))"
          result))

(defn python-result
  [repo module fname & args]
  (let [{:keys [deps o]} (apply build-python-payload args)
        dependencies (->> [(str "import " module)
                           "import sys"
                           (install-robust-encoder)
                           "import json"
                           "import jsonEncoder"]
                          (concat deps)
                          (string/join "; "))
        exec (format "%s.%s(%s)"
                     module fname o)
        command (format "%s; print %s"
                        dependencies (json-dumps exec))
        python-bash ["python" "-c"
                     command
                     :dir (io/file (io/resource (str "pythonLibs/" repo)))]
        {:keys [exit out] :as result} (apply shell/sh python-bash)]
    (-> result
        (assoc :error? (not (zero? exit)))
        (assoc :data (when (zero? exit)
                       (json/read-str out)))
        (assoc :python-bash python-bash))))

(def py (python-result "textSNE" "tsne" "Hbeta"
                       (i.core/matrix [[1 2] [3 4]])))
