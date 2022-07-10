(ns build
  (:refer-clojure :exclude [test])
  (:require
   [clojure.string :as str]
   [clojure.tools.build.api :as b]
   [org.corfield.build :as bb]))

(def ^:private lib 'io.github.sgepigon/mayo)
(def ^:private version "0.1.0-SNAPSHOT")

(defn- sha
  [{:keys [dir path] :or {dir "."}}]
  (-> {:command-args (cond-> ["git" "rev-parse" "HEAD"]
                       path (conj "--" path))
       :dir (.getPath (b/resolve-path dir))
       :out :capture}
      b/process
      :out
      str/trim))

(defn- add-common-opts [opts]
  (assoc opts :lib lib :version version :tag (sha nil)))

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn clean "Run clean." [opts]
  (-> opts
      add-common-opts
      bb/clean))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      add-common-opts
      bb/run-tests
      bb/clean
      bb/jar))

(defn install "Install the JAR locally." [opts]
  (-> opts
      add-common-opts
      bb/install))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (-> opts
      add-common-opts
      bb/deploy))

(comment

  ;; alternatively, use MAJOR.MINOR.COMMITS:
  (def version (format "1.0.%s" (b/git-count-revs nil))))
