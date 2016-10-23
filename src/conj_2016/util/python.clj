(ns conj-2016.util.python
  (:require clatrix.core
            [clojure.core.matrix :as m]
            [clojure.java.shell :as shell]
            [clojure.java.io :as io]
            [cheshire.core :as json]
            [cheshire.factory :as json.factory]
            [clojure.string :as string]))

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

(defmethod shell-parseable clojure.lang.PersistentVector
  [obj]
  (update (seq-parser obj)
          :o (partial format "[%s]")))

(defn- matrix-parser
  [obj]
  (let [{:keys [deps o]} (shell-parseable (m/to-nested-vectors obj))]
    {:deps (conj deps "import numpy")
     :o (format "numpy.array(%s)"
                o)}))

(defmethod shell-parseable mikera.matrixx.Matrix
  [obj]
  (matrix-parser obj))

(defmethod shell-parseable clatrix.core.Matrix
  [obj]
  (matrix-parser obj))

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

(defn- parse-json
  [json-string]
  (binding [json.factory/*json-factory* (json.factory/make-json-factory
                                          {:allow-non-numeric-numbers true})]
    (json/decode json-string)))

(defn python-inline-result
  ([repo module code] (python-inline-result repo module code []))
  ([repo module code deps]
   (let [dependencies (->> [(str "import " module)
                            "import sys"
                            (install-robust-encoder)
                            "import json"
                            "import jsonEncoder"]
                           (concat deps)
                           (string/join "; "))
         command (format "%s; print %s"
                         dependencies (json-dumps code))
         python-bash ["python" "-c"
                      command
                      :dir (io/file (io/resource (str "pythonLibs/" repo)))]
         {:keys [exit out] :as result} (apply shell/sh python-bash)
         data (when (zero? exit)
                (try
                  (parse-json out)
                  (catch Exception e
                    e)))]
     (-> result
         (assoc :success? (and (zero? exit)
                               (not (instance? Exception data))))
         (assoc :data data)
         (assoc :python-bash python-bash)))))

(defn python-result
  [repo module fname & args]
  (let [{:keys [deps o]} (apply build-python-payload args)
        code (format "%s.%s(%s)"
                     module fname o)]
    (python-inline-result repo module code deps)))
