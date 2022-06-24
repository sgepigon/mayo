(ns sgepigon.mayo.instrument.specs
  (:require
   [clojure.spec.alpha :as s]
   [sgepigon.mayo.instrument :as-alias is]))

(s/def ::sym qualified-symbol?)
(s/def ::raw fn?)

(s/def ::request
  (s/keys :req-un [::sym ::raw]))

(s/def ::is/context
  (s/keys :req-un [::request]))
