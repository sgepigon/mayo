(ns build
  (:refer-clojure :exclude [test])
  (:require
   [org.corfield.build :as bb]))

(def ^:private lib 'io.github.sgepigon/mayo)
(def ^:private version "0.1.0-SNAPSHOT")

(defn test "Run the tests." [opts]
  (bb/run-tests opts))

(defn clean "Run clean." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/clean)))

(defn ci "Run the CI pipeline of tests (and build the JAR)." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/run-tests)
      (bb/clean)
      (bb/jar)))

(defn install "Install the JAR locally." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

(defn deploy "Deploy the JAR to Clojars." [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/deploy)))

(comment

  (require '[clojure.tools.build.api :as b]) ; for b/git-count-revs

  ;; alternatively, use MAJOR.MINOR.COMMITS:
  (def version (format "1.0.%s" (b/git-count-revs nil))))
