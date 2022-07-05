(ns sgepigon.mayo.interceptor
  (:require
   [sgepigon.mayo.interceptor.impl :as ic.impl]))

;;;; API

(defn enqueue
  "Add `interceptors` to `context`'s queue."
  [context interceptors]
  (update context ::queue ic.impl/into-queue interceptors))

(defn terminate
  "Short circuit into the :leave stage."
  [context]
  (ic.impl/terminate context))

(defn execute
  "Execute the interceptor chain."
  [context interceptors]
  (-> context
      (enqueue interceptors)
      ic.impl/enter
      ic.impl/leave))
