(ns conj-2016.util.python
  "A namespace dedicated to being able to run clojure in python."
  (:require clatrix.core
            [cheshire.core :as json]
            [cheshire.factory :as json.factory]
            [clojure.core.matrix :as m]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as string]
            [taoensso.timbre :as log])
  (:import [java.io File]))

(def spat-files-prepend (str (ns-name *ns*) "_spat_command"))
(def spat-file-location "/tmp/")
(def spat-limit (-> (shell/sh "getconf" "ARG_MAX")
                    :out
                    string/trim
                    Integer/parseInt))

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
             (string/join ","))}))

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

(defmethod shell-parseable mikera.vectorz.AVector
  [obj]
  (matrix-parser obj))

(defmethod shell-parseable mikera.arrayz.impl.SliceArray
  [obj]
  (matrix-parser obj))

(defmethod shell-parseable String
  [obj]
  {:o (string/escape obj {\" "\\\"" \' "\\'"})})

(defmethod shell-parseable Double
  [^Double obj]
  (cond
    (.isInfinite obj)
    {:deps ["import numpy"]
     :o (str (when (neg? obj) "-") "numpy.inf")}

    :else {:o (str obj)}))

(defmethod shell-parseable Boolean
  [obj]
  {:o (if obj
        "True"
        "False")})

(defmethod shell-parseable nil
  [obj]
  {:o "None"})

(defmethod shell-parseable :default
  [obj]
  {:o (str obj)})

(defn- build-python-payload
  [& args]
  (seq-parser args))

(defn- install-robust-encoder
  []
  (format "sys.path.append('%s')"
          (.getParent (io/file (io/resource "jsonEncoder.py")))))

(defn- json-dumps
  [result]
  (format "json.dumps(%s, cls=jsonEncoder.RobustEncoder, separators=(',',':'))"
          result))

(defn- parse-json
  [json-string]
  (binding [json.factory/*json-factory* (json.factory/make-json-factory
                                          {:allow-non-numeric-numbers true})]
    (json/decode json-string)))

(defn- spat?
  "Given a python command optionally spit it to a /tmp/ file
  when it would cause an:

  java.io.IOException: error=7, Argument list too long
  java.io.IOException: Cannot run program \"python\""
  [command dir]
  (if (>= spat-limit (count command))
    command
    (let [file-name (str spat-file-location (gensym spat-files-prepend))
          file (io/file file-name)]
      (spit file
            (format "import sys\nsys.path.append('%s')\n"
                    (str dir)))
      (spit file command :append true)
      file)))

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
         dir (io/file (io/resource (str "pythonLibs/" repo)))
         command (spat? (format "%s; print %s"
                                dependencies (json-dumps code))
                        dir)
         python-bash (remove
                       nil?
                       ["python" (when-not (instance? File command)
                                   "-c")
                        (str command)
                        :dir dir])]
     (try
       (let [{:keys [exit out] :as result} (apply shell/sh python-bash)
             data (when (zero? exit)
                    (try
                      (-> out
                          string/split-lines
                          last
                          parse-json)
                      (catch Exception e
                        e)))
             to-return (-> result
                           (assoc :success? (and (zero? exit)
                                                 (not (instance? Exception data))))
                           (assoc :data data)
                           (assoc :python-bash python-bash))]
         (when (instance? File command)
           (io/delete-file command))
         to-return)
       (catch java.io.IOException e
         (log/error e)
         (log/error "Leaving file alone to be able to review")
         (log/error python-bash)
         (throw e))
       (catch Exception e
         (log/error e {:repo repo
                       :module module
                       :code code
                       :deps deps
                       :dependencies dependencies
                       :dir dir
                       :command command
                       :python-bash python-bash})
         (throw e))))))

(defn python-result
  [repo module fname & args]
  (let [{:keys [deps o]} (apply build-python-payload args)
        code (format "%s.%s(%s)"
                     module fname o)]
    (python-inline-result repo module code deps)))
