(ns sgepigon.mayo.instrument.specs
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.instrument :as-alias is]
   [sgepigon.mayo.interceptor :as-alias ic]
   [sgepigon.mayo.interceptor.impl :as-alias ic.impl]
   [sgepigon.mayo.interceptor.specs :as-alias ic.specs]))

(s/def ::sym qualified-symbol?)
(s/def ::syms (s/coll-of ::sym))
(s/def ::raw fn?)

(s/def ::request
  (s/keys :req-un [::sym]
          :opt-un [::raw]))

(s/def ::is/context
  (s/keys :req-un [::request]))

(s/def ::sym-ics
  (s/map-of ::sym ::ic.specs/interceptors))

;;;; interceptor.impl

(s/def ::ic.impl/request
  (s/keys :req-un [::sym ::ic.specs/interceptors]))
(s/def ::ic.impl/response any?)
(s/def ::is/instrument
  (s/nilable boolean?))

(s/def ::ic.impl/context
  (s/keys :req-un [::ic.impl/request]
          :opt-un [::ic.impl/response ::is/instrument]
          :opt [::ic/queue ::ic/stack ::ic/errors]))

(s/def ::ic.impl/registry
  (s/and #(-> % meta ::is/registry)
         (s/map-of ::sym ::ic.impl/context)))

;;;; API

(s/fdef is/instrument
  :args (s/cat :syms-ics ::sym-ics
               :opts (s/? (s/keys)))
  :ret ::syms)

(s/fdef is/unstrument
  :args (s/cat :syms (s/? (s/alt :syms ::syms
                                 :registry ::ic.impl/registry)))
  :ret ::syms)
