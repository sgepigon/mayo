(ns sgepigon.mayo.extensions.spec.alpha
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.interceptor :as ic]))

;;;; Implementation

;; fspec

(defn- ->args [ctx] (-> ctx :request :args))
(defn- ->ret [ctx] (-> ctx :response :ret))
(defn- ->fn [ctx] {:args (->args ctx) :ret (->ret ctx)})

(defn- stage-fn
  "Return a function that can be used for an interceptor stage.

  * `fspec-k`: the relevant kwarg of the `s/fspec` (:args, :ret, or :fn)
  * `data-fn`: a function to extract the relevant values from a context
  * `ctx-fn`: an optional function to apply to the resulting context before
  stage-fn returns"
  ([fspec-k data-fn]
   (stage-fn fspec-k data-fn nil))
  ([fspec-k data-fn ctx-fn]
   (fn validate-fn [{{:keys [sym]} :request :as ctx}]
     (if-let [ed (some-> sym s/get-spec fspec-k (s/explain-data (data-fn ctx)))]
       (let [msg (str "Call to " sym " did not conform to spec.")
             data (cond-> ed
                    true (assoc ::s/fn sym)
                    true (assoc ::s/failure :instrument)
                    (identical? :args fspec-k) (assoc ::s/args (::s/value ed))
                    (identical? :ret fspec-k) (assoc ::s/ret (::s/value ed)))
             error {:msg msg :data data}]
         (cond-> ctx
           true (assoc :error error)
           ctx-fn ctx-fn))
       ctx))))

(defn fspec
  "An interceptor for instrumenting clojure.spec fspecs.

  Takes `opts` map which keys can include:
  * fspec keys (`:args`, `:ret`, `:fn`): When truthy, enable checking of that
  key.
  * `:fail-fast?`: Skips running the function if :args spec fails (default
  true).

  A 0-arity call defaults to `stest/instrument`'s behavior of only checking the
  :args spec."
  ([]
   (fspec {:args true}))
  ([opts]
   (let [{:keys [fail-fast?] :or {fail-fast? true}} opts
         halt-fn (when fail-fast?
                   #(ic/halt % fail-fast?))]
     (cond-> {:name ::fspec}
       (:args opts) (assoc :enter (stage-fn :args ->args halt-fn))
       (:ret opts) (assoc :leave (stage-fn :ret ->ret))
       (:fn opts) (update :leave (fn [f]
                                   (cond-> (stage-fn :fn ->fn)
                                     (some? f) (comp f))))))))
