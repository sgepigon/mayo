(ns ^:no-doc sgepigon.mayo.interceptor.impl
  (:require
   [sgepigon.mayo.interceptor :as-alias ic]))

;;;; Implementation

(defn skip? [context]
  (contains? context ::ic/skip?))

(defn stage
  "Shared logic for executing stages."
  [context stage-k from-k to-k]
  (letfn [(advance [context interceptor]
            (-> context
                (update from-k pop)
                (update to-k (fnil conj []) interceptor)))
          (errors [context interceptor t]
            (update context
                    ::ic/errors
                    (fnil conj [])
                    {:stage-k stage-k :interceptor interceptor :t t}))]
    (loop [{from from-k :as ctx} context]
      (if-let [{stage-fn stage-k :as interceptor} (peek from)]
        (let [advanced (advance ctx interceptor)]
          (if stage-fn
            (recur (try (stage-fn advanced)
                        (catch Throwable t
                          (errors advanced interceptor t))))
            (recur advanced)))
        ctx))))

(defn enter [context]
  (stage context :enter ::ic/queue ::ic/stack))

(defn leave [context]
  (stage context :leave ::ic/stack ::ic/finished))

(defn execute-fn [{:keys [request] :as context}]
  (if (skip? context)
    context
    (try
      (let [ret (apply (:raw request) (:args request))]
        (assoc-in context [:response :ret] ret))
      (catch Throwable t
        (assoc context :error (Throwable->map t))))))

(defn terminate
  "Short circuit into the :leave stage."
  [{::ic/keys [queue] :as context}]
  (-> context
      (update ::ic/stack (fnil into []) queue)
      (update ::ic/queue empty)))
