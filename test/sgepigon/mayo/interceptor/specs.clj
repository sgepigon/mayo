(ns sgepigon.mayo.interceptor.specs
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.instrument.specs :as-alias is.specs]
   [sgepigon.mayo.interceptor :as-alias ic]
   [sgepigon.mayo.interceptor.impl :as-alias ic.impl]))

;;;; Inteceptor specs

(s/def ::name qualified-keyword?)
;; TODO Should these be `fn?` or `ifn?`?
(s/def ::enter fn?)
(s/def ::leave fn?)
(s/def ::error fn?)

(s/def ::interceptor
  (s/keys :opt-un [::name ::enter ::leave ::error]))
(s/def ::interceptors
  (s/coll-of ::interceptor))

;;;; Context specs

(s/def ::args coll?)
(s/def ::ret any?)

(s/def ::ic/queue (s/nilable ::interceptors))
(s/def ::ic/stack ::interceptors)

(s/def ::request
  (s/merge ::is.specs/request
           (s/keys :opt-un [::args])))

(s/def ::response
  (s/keys :opt-un [::ret]))

(s/def ::context
  (s/keys :req-un [::request]
          :opt-un [::response]
          :opt [::ic/queue ::ic/stack]))

;;;; ic.impl

(s/def ::ic.impl/stage-k #{:enter :leave :error})
(s/def ::ic.impl/from-k #{::ic/queue ::ic/stack})
(s/def ::ic.impl/to-k #{::ic/stack})
(s/def ::ic.impl/happy-path ifn?)
(s/def ::ic.impl/unhappy-path ifn?)

(s/fdef ic.impl/stage-1
  :args (s/cat :context ::context
               :stage-k ::ic.impl/stage-k
               :from-k ::ic.impl/from-k
               :to-k (s/nilable ::ic.impl/to-k)
               :happy-path (s/nilable ::ic.impl/happy-path)
               :unhappy-path (s/nilable ::ic.impl/unhappy-path))
  :ret ::context)

(s/fdef ic.impl/stage
  :args (s/cat :context ::context
               :k ::ic.impl/stage-k)
  :ret ::context)

;;;; API

(s/fdef ic/enqueue
  :args (s/cat :context ::context
               :interceptors ::interceptors)
  :ret ::context)

(s/fdef ic/terminate
  :args (s/cat :context ::context)
  :ret ::context)

(s/fdef ic/execute
  :args (s/cat :context ::context
               :interceptors ::interceptors)
  :ret ::context)
