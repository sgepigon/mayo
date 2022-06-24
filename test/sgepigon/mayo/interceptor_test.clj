(ns sgepigon.mayo.interceptor-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.test :refer [deftest is testing]]
   [sgepigon.mayo.experimental.interceptors :as exp.ics]
   [sgepigon.mayo.interceptor :as ic]))

;;;; Helper functions

(defn instrument-ns
  "Helper fn to run `stest/instrument` for the namespace aliased by `ns-alias`.

  Targets fspecs by namespace as I want to avoid `stest/instrument`ing the
  functions I'm testing my instrumentaiton on e.g. `test-fn`.

  Until I get interceptors working nicely on my own functions, leverage existing
  `stest/instrument` for debugging."
  [ns-alias]
  (stest/instrument
   (into #{}
         (filter (fn [sym]
                   (= (-> *ns* ns-aliases (get ns-alias) str)
                      (namespace sym))))
         (stest/instrumentable-syms))))

(instrument-ns 'ic)

(defn- wrap->ctx
  "Helper fn: much of this logic is inlined at `is/instrument-1`."
  [sym & args]
  (when-let [resolved (and (qualified-symbol? sym)
                           (requiring-resolve sym))]
    {:request (-> {:sym sym :raw @resolved}
                  (assoc :args args))}))

;;;; Tests

(s/fdef test-fn
  :args (s/cat :x int?)
  :ret number?)

(defn test-fn
  "Simple fn to test instrumentation."
  [x]
  (inc x))

(deftest errors-test
  (let [args '(1)
        badceptor {:name ::badceptor
                   :enter #(update % ::badceptor inc)
                   :leave #(update % ::badceptor inc)}
        executed (-> (apply wrap->ctx `test-fn args)
                     (ic/execute [badceptor]))]
    (testing "Issues with interceptors themselves are captured in
    `::ic/errors.`"
      (is (= (apply test-fn args) (-> executed :response :ret)))
      (testing "Both `badceptor`'s `:enter` and `:leave` should be captured in
        `::ic/errors`."
        (is (= 2 (count (::ic/errors executed))))
        (is (= {::badceptor #{:enter :leave}}
               (reduce (fn [acc {:keys [interceptor stage-k]}]
                         (update acc (:name interceptor) (fnil conj #{}) stage-k))
                       {}
                       (::ic/errors executed))))))))

(comment

  (ic/execute (wrap->ctx `test-fn 2.0)
              [(exp.ics/fspec {:args true}) exp.ics/no-op])

  (keys (ns-publics *ns*)))
