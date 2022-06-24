(ns sgepigon.mayo.interceptor
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.interceptor.specs :as ic.specs]))

;;;; Implementation

(defn- halt? [context]
  (and (empty? (::queue context))
       (empty? (::stack context))))

(defn- skip? [context]
  (contains? context ::skip?))

(defn- stage
  "Shared logic for executing stages."
  [context stage-k from-k to-k]
  (letfn [(advance [context interceptor]
            (-> context
                (update from-k pop)
                (update to-k (fnil conj []) interceptor)))
          (errors [context interceptor t]
            (update context
                    ::errors
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

(defn- enter [context]
  (stage context :enter ::queue ::stack))

(defn- leave [context]
  (stage context :leave ::stack ::finished))

(defn- execute-fn [{:keys [request] :as context}]
  (if (skip? context)
    context
    (try
      (let [ret (apply (:raw request) (:args request))]
        (assoc-in context [:response :ret] ret))
      (catch Throwable t
        (assoc context :error (Throwable->map t))))))

;;;; API

(s/fdef enqueue
  :args (s/cat :context ::ic.specs/context
               :interceptors (s/coll-of ::ic.specs/interceptor))
  :ret ::ic.specs/context)

(defn enqueue
  "Add `interceptors` to `context`'s queue."
  [context interceptors]
  (update context
          ::queue
          (fnil into (clojure.lang.PersistentQueue/EMPTY))
          interceptors))

(s/fdef terminate
  :args (s/cat :context ::ic.specs/context)
  :ret ::ic.specs/context)

(defn terminate
  "Short circuit into the :leave stage."
  [{::keys [queue] :as context}]
  (-> context
      (update ::stack (fnil into []) queue)
      (update ::queue empty)))

(s/fdef halt
  :args (s/cat :context ::ic.specs/context
               :skip? (s/? boolean?))
  :ret (s/and ::ic.specs/context
              halt?))

(defn halt
  "Short circuit, avoiding remaining interceptors in :enter or :leave stages.

  Optionally takes a `skip?` boolean to determine whether or not to run the
  function."
  ([context]
   (halt context false))
  ([context skip?]
   (-> context
       (update ::stack empty)
       (update ::queue empty)
       (assoc ::skip? skip?))))

(s/fdef execute
  :args (s/cat :context ::ic.specs/context
               :interceptors (s/coll-of ::ic.specs/interceptor))
  :ret ::ic.specs/context)

(defn execute
  "Execute the interceptor chain."
  [context interceptors]
  (-> context (enqueue interceptors) enter execute-fn leave))

(defn run
  "Return the function output from `context`."
  [context]
  (if-let  [{:keys [msg data]} (:error context)]
    (throw (ex-info msg (assoc data ::context (dissoc context :error))))
    (-> context :response :ret)))