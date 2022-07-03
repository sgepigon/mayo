(ns sgepigon.mayo.instrument-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [sgepigon.mayo.experimental.interceptors :as exp.ics]
   [sgepigon.mayo.extensions.spec.alpha :as ext.s]
   [sgepigon.mayo.instrument :as is]
   [sgepigon.mayo.instrument.specs]
   [sgepigon.mayo.interceptor :as ic]
   [sgepigon.mayo.interceptor-test :as ic-test]))

(ic-test/instrument-ns 'ic)
(ic-test/instrument-ns 'is)

(defn- instrumented?
  "Returns true if a var is instrumented."
  [v]
  ;; TODO Put instrumented metadata var and function? Alternatively I can see if
  ;; the symbol is in `is/registry`
  (contains? (meta v) ::is/context))

(deftest *nstrument-test
  (let [test-syms [`ic-test/test-fn]
        raw @#'ic-test/test-fn
        ics [(ext.s/fspec {:args true :ret true :fn true})
             exp.ics/no-op]
        instrumented-syms (is/instrument
                           (into {} (map (fn [v] [v ics])) test-syms))
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
            "If `ext.s/fspec` fails on `:args`, the function shouldn't run at
            all i.e. no `:response` map.")))
    (let [unstrumented-syms (is/unstrument)]
      (testing "Unstrumentation"
        (is (= test-syms unstrumented-syms))
        (is (not (instrumented? #'ic-test/test-fn))
            "The var's metadata for instrumentation should be removed.")
        (is (identical? raw @#'ic-test/test-fn)
            "The original function should be restored after unstrumentation.")
        (is (= (raw 2.0) (#'ic-test/test-fn 2.0))
            "`ext.s/fspec` should have no effect on unstrumented
            functions.")))))
