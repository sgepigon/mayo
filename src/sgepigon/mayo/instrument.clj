(ns sgepigon.mayo.instrument
  (:require
   [sgepigon.mayo.interceptor :as ic]))

;;;; Implementation

(defonce ^:private instrumented-vars (atom {}))

(defn- var->raw [v]
  (-> v meta ::context :request :raw))

(defn- instrument-1 [sym interceptors]
  (when-let [resolved (and (qualified-symbol? sym) (resolve sym))]
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
  (when-let [resolved (and (qualified-symbol? sym) (resolve sym))]
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
  [syms-ics]
  (locking instrumented-vars
    (into []
          (keep (fn [[sym interceptors]]
                  (when (instrument-1 sym interceptors)
                    (swap! instrumented-vars assoc sym interceptors)
                    sym)))
          syms-ics)))

(defn unstrument
  "Remove instrumentation for a collection of namespace-qualified symbols `syms`.

  If called with no args, unstrument all the instrumented symbols known by mayo.
  Returns a collection of syms naming the vars unstrumented."
  ([]
   (unstrument (map first @instrumented-vars)))
  ([syms]
   (locking instrumented-vars
     (into []
           (keep (fn [sym]
                   (when (unstrument-1 sym)
                     (swap! instrumented-vars dissoc sym)
                     sym)))
           syms))))

(comment
  (require '[clojure.spec.test.alpha :as stest])

  (stest/instrument)

  (stest/unstrument)

  (stest/instrumentable-syms))
