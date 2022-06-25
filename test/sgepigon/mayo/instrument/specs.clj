(ns sgepigon.mayo.instrument.specs
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.instrument :as-alias is]
   [sgepigon.mayo.interceptor.specs :as-alias ic.specs]))

(s/def ::sym qualified-symbol?)
(s/def ::raw fn?)

(s/def ::request
  (s/keys :req-un [::sym ::raw]))

(s/def ::is/context
  (s/keys :req-un [::request]))

(s/fdef instrument
  :args (s/cat :syms-ics (s/map-of ::sym
                                   (s/coll-of ::ic.specs/interceptor)))
  :ret (s/coll-of ::sym))

(s/fdef unstrument
  :args (s/cat :syms (s/? (s/coll-of ::sym)))
  :ret (s/coll-of ::sym))
