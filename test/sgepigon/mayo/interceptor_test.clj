(ns sgepigon.mayo.interceptor-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.test :refer [deftest is testing]]
   [sgepigon.mayo.experimental.interceptors :as exp.ics]
   [sgepigon.mayo.extensions.spec.alpha :as ext.s]
   [sgepigon.mayo.interceptor :as ic]
   [sgepigon.mayo.interceptor.specs]))

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
        ics [badceptor]
        executed (-> (apply wrap->ctx `test-fn args)
                     (ic/execute ics))]
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

(deftest fspec-test
  (let [test-sym `test-fn
        default-ics [(ext.s/fspec)]
        no-fail-fast-ics [(ext.s/fspec {:args true :fail-fast? false})]
        valid-args '(1)
        invalid-args '(1.0)
        valid-ctx (apply wrap->ctx test-sym valid-args)
        invalid-ctx (apply wrap->ctx test-sym invalid-args)
        valid-executed (ic/execute valid-ctx default-ics)
        invalid-executed (ic/execute invalid-ctx default-ics)
        no-fail-fast-executed (ic/execute invalid-ctx no-fail-fast-ics)]
    (testing "`ext.s/fspec` doesn't affect `:request` or `:response` values."
      (is (= test-sym
             (-> valid-executed :request :sym)
             (-> invalid-executed :request :sym)
             (-> no-fail-fast-executed :request :sym)))
      (is (= (:request valid-ctx) (:request valid-executed)))
      (is (= (:request invalid-ctx)
             (:request invalid-executed)
             (:request no-fail-fast-executed)))
      (is (== (apply test-fn valid-args)
              (-> valid-executed :response :ret)
              (-> no-fail-fast-executed :response :ret))))
    (testing "Default `ext.s/fspec` fails fast and does not produce a
    `:response` map."
      (is (::ic/skip? invalid-executed))
      (is (not (::ic/skip? no-fail-fast-executed)))
      (is (not (contains? invalid-executed :response)))
      (is (contains? no-fail-fast-executed :response)))
    (testing "`:error` data"
      (is (not (contains? valid-executed :error)))
      (is (contains? invalid-executed :error))
      (is (contains? no-fail-fast-executed :error))
      (is (= (str "Call to " test-sym " did not conform to spec.")
             (-> invalid-executed :error :msg)
             (-> no-fail-fast-executed :error :msg)))
      (is (= invalid-args
             (-> invalid-executed :request :args)
             (-> no-fail-fast-executed :request :args)
             (-> invalid-executed :error :data ::s/args)
             (-> no-fail-fast-executed :error :data ::s/args)))
      (testing "`stest/instrument` compatibility"
        (is (= :instrument
               (-> invalid-executed :error :data ::s/failure)
               (-> no-fail-fast-executed :error :data ::s/failure))
            "`:clojure.spec.alpha/failure :instrument`")))))

(comment

  (stest/unstrument)

  (ic/execute (wrap->ctx `test-fn 2.0)
              [(ext.s/fspec {:args true}) exp.ics/no-op])

  (keys (ns-publics *ns*)))
