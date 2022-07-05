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

(defn stage
  "Shared logic for executing stages."
  ([context stage-k from-k]
   (stage context stage-k from-k nil))
  ([context stage-k from-k to-k]
   (letfn [(recovered? [context stage-k]
             (and (identical? :error stage-k)
                  (not (:error context))))
           (advance [context stage-k interceptor]
             (if (recovered? context stage-k)
               (stage context :leave ::ic/stack)
               (cond-> context
                 true (update from-k pop)
                 to-k (update to-k push-stack interceptor))))
           (error [context interceptor e]
             (let [msg (cond-> "Interceptor error"
                         (ex-message e) (str ": " (ex-message e)))
                   data (merge {:stage stage-k
                                :interceptor interceptor}
                               (ex-data e))
                   err (ex-info msg data e)]
               (-> context
                   terminate
                   (assoc :error err)
                   (stage :error ::ic/stack))))]
     (loop [{from from-k :as ctx} context]
       (if-let [{stage-fn stage-k :as interceptor} (peek from)]
         (if stage-fn
           (recur (try (-> ctx stage-fn (advance stage-k interceptor))
                       (catch Exception e
                         (error context interceptor e))))
           (recur (advance ctx stage-k interceptor)))
         ctx)))))

(defn enter [context]
  (stage context :enter ::ic/queue ::ic/stack))

(defn leave [context]
  (stage context :leave ::ic/stack))

(def handler
  "Function interceptor handler as an interceptor."
  {:enter (fn enter [{{:keys [raw args]} :request :as context}]
            (let [ret (apply raw args)]
              (assoc-in context [:response :ret] ret)))})
