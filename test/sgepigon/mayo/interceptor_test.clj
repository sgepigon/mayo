(ns sgepigon.mayo.interceptor-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.spec.test.alpha :as stest]
   [clojure.test :refer [deftest is testing]]
   [sgepigon.mayo.experimental.interceptors :as exp.ics]
   [sgepigon.mayo.extensions.spec.alpha :as ext.s]
   [sgepigon.mayo.interceptor :as ic]
   [sgepigon.mayo.interceptor.impl :as ic.impl]
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

(def bad
  "Testing exceptions caused by errors in interceptors themselves."
  {:name ::bad
   :enter #(update % ::bad inc)})

(def recover
  "Test that :leave stages resume if an :error is handled."
  {:name ::recover
   :error #(dissoc % :error)
   :leave #(assoc % ::recover true)})

(deftest bad-interceptor-test
  (let [args '(1)
        ics [bad ic.impl/handler]
        executed (-> (apply wrap->ctx `test-fn args)
                     (ic/execute ics))]
    (testing "`bad`'s stage `:enter` should be captured in `:error`."
      (is (= "Interceptor error" (-> executed :error ex-message)))
      (is (= ::bad (-> executed :error ex-data :interceptor :name)))
      (is (= :enter (-> executed :error ex-data :stage))))))

(deftest recover-test
  (let [args '(1)
        ics [bad {:leave #(assoc % ::other-leave-run? true)} recover ic.impl/handler]
        executed (-> (apply wrap->ctx `test-fn args)
                     (ic/execute ics))]
    (testing "`bad`'s stage `:enter` should be captured in `:error`."
      (is (nil? (:error executed)))
      (is (true? (::recover executed)))
      (is (true? (::other-leave-run? executed))
          "`::other-leave-run?` runs after `recover`.")
      (is (nil? (-> executed :response :ret))
          "`ic.impl/handler` shouldn't run because its leave runs before `recover`."))))

(deftest fspec-test
  (let [test-sym `test-fn
        default-ics [(ext.s/fspec) ic.impl/handler]
        valid-args '(1)
        invalid-args '(1.0)
        valid-ctx (apply wrap->ctx test-sym valid-args)
        invalid-ctx (apply wrap->ctx test-sym invalid-args)
        valid-executed (ic/execute valid-ctx default-ics)
        invalid-executed (ic/execute invalid-ctx default-ics)]
    (testing "`ext.s/fspec` doesn't affect `:request` or `:response` values."
      (is (= test-sym
             (-> valid-executed :request :sym)
             (-> invalid-executed :request :sym)))
      (is (= (:request valid-ctx) (:request valid-executed)))
      (is (= (:request invalid-ctx)
             (:request invalid-executed)))
      (is (== (apply test-fn valid-args)
              (-> valid-executed :response :ret))))
    (testing "`ext.s/fspec` fails fast and does not produce a `:response` map."
      (is (not (contains? invalid-executed :response))))
    (testing "`:error` data"
      (is (not (contains? valid-executed :error)))
      (is (contains? invalid-executed :error))
      (is (= (str "Call to " test-sym " did not conform to spec.")
             (-> invalid-executed :error ex-message)))
      (is (= invalid-args
             (-> invalid-executed :request :args)
             (-> invalid-executed :error ex-data ::s/args)))
      (testing "`stest/instrument` compatibility"
        (is (= :instrument
               (-> invalid-executed :error ex-data ::s/failure))
            "`:clojure.spec.alpha/failure :instrument`")))))

(comment

  (stest/unstrument)

  (ic/execute (wrap->ctx `test-fn 2.0)
              [(ext.s/fspec {:args true}) exp.ics/no-op ic.impl/handler])

  (keys (ns-publics *ns*)))
