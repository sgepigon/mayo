(ns sgepigon.mayo.instrument
  (:require
   [sgepigon.mayo.interceptor :as ic]
   [sgepigon.mayo.interceptor.impl :as ic.impl]))

;;;; Implementation

(defonce ^{:private true
           :doc "The context between instrument and unstrument."}
  registry
  (atom ^::registry {}))

(defn- register! [sym context]
  (swap! registry assoc sym (assoc context ::instrument true)))

(defn- unregister! [sym]
  (swap! registry update sym dissoc ::instrument))

(defn- var->raw [v]
  (-> v meta ::context :request :raw))

(defn- instrument-1 [sym interceptors]
  (when-let [resolved (resolve sym)]
    (when-not (-> resolved meta :macro)
      (alter-var-root
       resolved
       (fn [f]
         (let [raw (or (var->raw resolved) f)
               ctx {:request {:sym sym :raw raw}}]
           (alter-meta! resolved assoc ::context ctx)
           (fn wrap [& args]
             (-> ctx
                 (assoc-in [:request :args] args)
                 (ic/execute interceptors)
                 ic/run))))))))

(defn- unstrument-1 [sym]
  (when-let [resolved (resolve sym)]
    (alter-var-root
     resolved
     (fn [f]
       (let [raw (or (var->raw resolved) f)]
         (alter-meta! resolved dissoc ::context)
         raw)))))

;;;; API

(defn instrument
  "Takes a map of namespace-qualified symbols to collection of interceptors.

  Returns a collection of syms naming the vars instrumented."
  ([syms-ics]
   (instrument syms-ics {}))
  ([syms-ics opts]
   (let [is-ics (or (:interceptors opts) [])]
     (locking registry
       (into []
             (comp
              (filter (comp qualified-symbol? first))
              (map (fn execute [[sym fn-ics]]
                     (-> {:request {:sym sym :interceptors fn-ics}}
                         (ic/enqueue is-ics)
                         ic.impl/enter)))
              (keep (fn [{{:keys [sym interceptors]} :request :as ctx}]
                      (when (instrument-1 sym interceptors)
                        (register! sym ctx)
                        sym))))
             syms-ics)))))

(defn unstrument
  "Remove instrumentation for a collection of namespace-qualified symbols `syms`.

  If called with no args, unstrument all the instrumented symbols known by mayo.
  Returns a collection of syms naming the vars unstrumented."
  ([]
   (unstrument @registry))
  ([syms]
   (locking registry
     (into []
           (comp
            (filter (comp ::instrument second))
            (map (fn execute [[_ context]]
                   (ic.impl/leave context)))
            (keep (fn [{{:keys [sym]} :request}]
                    (when (unstrument-1 sym)
                      (unregister! sym)
                      sym))))
           (cond-> @registry
             (-> syms meta ::registry not) (select-keys syms))))))

(defn errors
  "Return a collection of errors instrumenting or unstrumenting `sym`."
  [sym]
  (get-in @registry [sym ::ic/errors]))

(comment
  (require '[clojure.spec.test.alpha :as stest])

  (stest/instrument)

  (stest/unstrument)

  (stest/instrumentable-syms))
