(ns sgepigon.mayo.experimental.interceptors
  "Namespace for example implementations of interceptors.

  NOTE: This is under `test/` because these are unofficial interceptors (i.e. I
  don't want them as a part of the public API)."
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.interceptor :as ic]))

(def no-op
  "Effectively a no-op interceptor."
  {:name ::no-op
   :enter identity
   :leave identity})

(defn sample
  "Return an interceptor that short-circuits the chain for `sampling-rate`."
  [sampling-rate]
  {:enter (fn [ctx]
            (if (< (rand) sampling-rate)
              ctx
              (ic/halt ctx)))})

(def fspec-basic
  "Basic instrumentation interceptor for clojure.spec.

  Can get the vanilla behavior of just `:args` fspec checking by `dissoc`ing
  `:leave`."
  {:name ::fspec-basic
   :enter (fn [{:keys [request] :as ctx}]
            (if-let [ed (some-> request :sym s/get-spec :args
                                (s/explain-data (:args request)))]
              (-> ctx
                  (assoc :error {:msg ":args spec failed"
                                 :map {:spec-failed ed}})
                  (ic/halt true))
              ctx))
   :leave (fn [{:keys [request response] :as ctx}]
            (if-let [ed (some-> request :sym s/get-spec :ret
                                (s/explain-data (:ret response)))]
              (assoc ctx :error {:msg ":ret spec failed"
                                 :map {:spec-failed ed}})
              ctx))})

(defn fspec
  "Slightly more sophisticated instrumentation interceptor for clojure.spec.

  Takes `opts`: a map of fspec keys (:args, :ret, :fn) to booleans. When truthy,
  enable checking of that key."
  [opts]
  (letfn [(->args [ctx] (-> ctx :request :args))
          (->ret [ctx] (-> ctx :response :ret))
          (->fn [ctx] {:args (->args ctx) :ret (->ret ctx)})
          (stage-fn
            ([fspec-k data-fn]
             (stage-fn fspec-k data-fn nil))
            ([fspec-k data-fn ctx-fn]
             (fn validate-fn [ctx]
               (if-let [ed (some-> ctx :request :sym s/get-spec fspec-k
                                   (s/explain-data (data-fn ctx)))]
                 (cond-> ctx
                   true (assoc :error {:msg (str fspec-k " spec failed")
                                       :data {:spec-failed ed}})
                   ctx-fn ctx-fn)
                 ctx))))]
    (cond-> {:name ::fspec}
      (:args opts) (assoc :enter (stage-fn :args ->args #(ic/halt % true)))
      (:ret opts) (assoc :leave (stage-fn :ret ->ret))
      (:fn opts) (update :leave (fn [f]
                                  (if (some? f)
                                    (comp (stage-fn :fn ->fn) f)
                                    (stage-fn :fn ->fn)))))))