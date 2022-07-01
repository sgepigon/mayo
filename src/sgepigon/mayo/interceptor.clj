(ns sgepigon.mayo.interceptor
  (:require
   [sgepigon.mayo.interceptor.impl :as ic.impl]))

;;;; API

(defn enqueue
  "Add `interceptors` to `context`'s queue."
  [context interceptors]
  (update context
          ::queue
          (fnil into (clojure.lang.PersistentQueue/EMPTY))
          interceptors))

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

(defn execute
  "Execute the interceptor chain."
  ([context interceptors]
   (-> context
       (enqueue interceptors)
       ic.impl/enter
       ic.impl/execute-fn
       ic.impl/leave)))

(defn run
  "Return the function output from `context`."
  [context]
  (if-let  [{:keys [msg data]} (:error context)]
    (throw (ex-info msg (assoc data ::context (dissoc context :error))))
    (-> context :response :ret)))
