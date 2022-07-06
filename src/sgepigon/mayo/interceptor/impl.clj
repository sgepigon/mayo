(ns ^:no-doc sgepigon.mayo.interceptor.impl
  (:require
   [sgepigon.mayo.interceptor :as-alias ic]))

;;;; Implementation

(defn empty-stack []
  [])
(defn empty-queue []
  (clojure.lang.PersistentQueue/EMPTY))

(defn push-stack [to interceptor]
  (conj (or to (empty-stack)) interceptor))

(defn into-stack [to from]
  (into (or to (empty-stack)) from))

(defn into-queue [to from]
  (into (or to (empty-queue)) from))

(defn terminate
  "Internal details. Use `ic/terminate` instead."
  [context]
  (-> context
      (update ::ic/stack into-stack (::ic/queue context))
      (dissoc ::ic/queue)))

(defn- stage-1
  "Passes `context` through stage `stage-k` for given `opts`.

  Required:

  * `from-k`: keyword to retrieve the next interceptor to process

  Nilable:

  * `to-k`: keyword for where to push the interceptor

  * `happy-path`: function to run when there is no `:error` in `context`

  * `unhappy-path`: function to run when there is an exception or `:error` in
  `context`"
  [context stage-k from-k to-k happy-path unhappy-path]
  (loop [{from from-k :as ctx} context]
    (if-let [{stage-fn stage-k :as interceptor} (peek from)]
      (letfn [(continue [ctx]
                (cond-> ctx
                  true (update from-k pop)
                  to-k (update to-k push-stack interceptor)))
              (chain-error [ctx e]
                (assoc ctx :error (ex-info
                                   (cond-> "Interceptor error"
                                     (ex-message e) (str ": " (ex-message e)))
                                   (into {:stage stage-k
                                          :interceptor interceptor}
                                         (ex-data e))
                                   e)))]
        (let [happy-path (or happy-path continue)
              unhappy-path (or unhappy-path continue)]
          (recur
           (if stage-fn
             (try
               (let [result (stage-fn ctx)]
                 (if (:error result)
                   (unhappy-path result)
                   (happy-path result)))
               (catch Exception e
                 (unhappy-path (chain-error ctx e))))
             (continue ctx)))))
      ctx)))

(defn- stage
  "Passes `context` through stage `k`."
  [ctx k]
  (case k
    :enter (stage-1 ctx k ::ic/queue ::ic/stack nil #(stage (terminate %) :error))
    :leave (stage-1 ctx k ::ic/stack nil nil #(stage % :error))
    :error (stage-1 ctx k ::ic/stack nil #(stage % :leave) nil)))

(defn enter [context]
  (stage context :enter))

(defn leave [context]
  (stage context :leave))

(def handler
  "Function interceptor handler as an interceptor."
  {:name ::handler
   :enter (fn enter [{{:keys [raw args]} :request :as context}]
            (let [ret (apply raw args)]
              (assoc-in context [:response :ret] ret)))})
