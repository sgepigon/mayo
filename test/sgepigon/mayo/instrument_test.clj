(ns sgepigon.mayo.instrument-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is testing]]
   [sgepigon.mayo.experimental.interceptors :as exp.ics]
   [sgepigon.mayo.extensions.spec.alpha :as ext.s]
   [sgepigon.mayo.instrument :as is]
   [sgepigon.mayo.instrument.specs]
   [sgepigon.mayo.interceptor :as ic]
   [sgepigon.mayo.interceptor-test :as ic-test]))

(ic-test/instrument-ns 'ic)
(ic-test/instrument-ns 'is)

(defn fdef-via-metadata-fn
  "Test function for `ext.s/fdef-via-metadata` instrument interceptor."
  [a _ {:as c} & {:as opts}]
  {:args `[boolean? int? (s/keys)]
   :ret `map?}
  (when a
    (merge c opts)))

(def ^:private expected-fdef-form
  `(s/fspec :args (s/cat :a boolean? :___1 int? :c (s/keys) :opts (s/keys*))
            :ret map?
            :fn nil))

(defn- instrumented?
  "Returns true if a var is instrumented."
  [v]
  ;; TODO Put instrumented metadata var and function? Alternatively I can see if
  ;; the symbol is in `is/registry`
  (contains? (meta v) ::is/context))

(deftest *nstrument-test
  (let [raw @#'ic-test/test-fn
        test-syms [`ic-test/test-fn]
        ics [(ext.s/fspec {:args true :ret true :fn true})
             exp.ics/no-op]
        syms-ics (zipmap test-syms (repeat ics))
        instrumented-syms (is/instrument syms-ics)
        metadata (meta #'ic-test/test-fn)]
    (testing "Instrumentation"
      (is (instrumented? #'ic-test/test-fn)
          "The var itself should be marked as instrumented.")
      (is (= test-syms instrumented-syms))
      (is (not (identical? raw @#'ic-test/test-fn))
          "The original function should be wrapped after instrumentation.")
      (is (= raw
             (#'is/var->raw #'ic-test/test-fn)
             (get-in metadata [::is/context :request :raw]))
          "The original function should be captured as `:raw` metadata.")
      (is (= (raw 1) (#'ic-test/test-fn 1))
          "Instrumented function gives the same result.")
      (testing "`ext.s/fspec`"
        (is (thrown? Exception (#'ic-test/test-fn 2.0))
            "`ext.s/fspec` checks `:args` satisfy `int?`.")
        (is (try (#'ic-test/test-fn 2.0)
                 (catch Exception e
                   (-> e ex-data ::ic/context :response nil?)))
            (str "If `ext.s/fspec` fails on `:args`,"
                 " the function shouldn't run at all"
                 " i.e. no `:response` map."))))
    (let [unstrumented-syms (is/unstrument)]
      (testing "Unstrumentation"
        (is (= test-syms unstrumented-syms))
        (is (not (instrumented? #'ic-test/test-fn))
            "The var's metadata for instrumentation should be removed.")
        (is (identical? raw @#'ic-test/test-fn)
            "The original function should be restored after unstrumentation.")
        (is (= (raw 2.0) (#'ic-test/test-fn 2.0))
            "`ext.s/fspec` should have no effect on unstrumented functions.")))))

(deftest instrument-interceptors-test
  (let [sym `fdef-via-metadata-fn
        test-syms [sym]
        fn-ics [(ext.s/fspec {:args true :ret true :fn true})]
        is-ics [(ext.s/fdef-via-metadata)]
        syms-ics (zipmap test-syms (repeat fn-ics))
        instrumented-syms (is/instrument syms-ics {:interceptors is-ics})
        valid-args [true 1 {:c/k 1} :opt/k 2]
        invalid-args [100 1 {:c/k 1} :opt/k 2]
        error-msg (format "Call to %s did not conform to spec." sym)
        get-error (fn get-error [sym]
                    (get-in #'is/registry [sym :error]))]
    (testing "`ext.s/fdef-via-metadata`"
      (is (= test-syms instrumented-syms))
      (is (every? nil? (map get-error instrumented-syms)))
      (is (= expected-fdef-form (-> sym s/get-spec s/form)))
      (is (= {:c/k 1 :opt/k 2} (apply #'fdef-via-metadata-fn valid-args)))
      (is (thrown? Exception (apply #'fdef-via-metadata-fn invalid-args)))
      (try (apply #'fdef-via-metadata-fn invalid-args)
           (catch Exception e
             (is (= sym (::s/fn (ex-data e))))
             (is (= error-msg (ex-message e)))
             (is (= [{:in [0] :path [:a] :pred `boolean? :val 100 :via []}]
                    (::s/problems (ex-data e))))))
      (let [unstrumented-syms (is/unstrument)]
        (is (= test-syms unstrumented-syms))
        (is (every? nil? (map get-error unstrumented-syms)))
        (is (nil? (s/get-spec sym))
            "Unregister fdef via metadata on :leave.")))))
