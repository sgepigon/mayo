(ns sgepigon.mayo.interceptor.specs
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.instrument.specs :as is.specs]
   [sgepigon.mayo.interceptor :as-alias ic]
   [sgepigon.mayo.interceptor.specs.errors :as-alias ic.specs.errors]))

;;;; Inteceptor specs

(s/def ::name qualified-keyword?)
;; TODO Should these be `fn?` or `ifn?`?
(s/def ::enter fn?)
(s/def ::leave fn?)
(s/def ::error fn?)

(s/def ::interceptor
  (s/keys :opt-un [::name ::enter ::leave ::error]))

;;;; Context specs

(s/def ::args coll?)
(s/def ::ret any?)

(s/def ::ic.specs.errors/stage-k #{:enter :leave})
(s/def ::ic.specs.errors/t any?)

(s/def ::ic.specs.errors/error
  (s/keys :req-un [::interceptor ::ic.specs.errors/stage-k ::ic.specs.errors/t]))

(s/def ::ic/errors
  (s/coll-of ::ic.specs.errors/error))
(s/def ::ic/queue
  (s/coll-of ::interceptor))
(s/def ::ic/stack
  (s/coll-of ::interceptor))

(s/def ::request
  (s/merge ::is.specs/request
           (s/keys :req-un [::args])))

(s/def ::response
  (s/keys :req-un [::ret]))

(s/def ::context
  (s/keys :req-un [::request]
          :opt-un [::response]
          :opt [::ic/queue ::ic/stack ::ic/errors]))
