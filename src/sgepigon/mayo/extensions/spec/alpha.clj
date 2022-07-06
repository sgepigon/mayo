(ns sgepigon.mayo.extensions.spec.alpha
  (:require
   [clojure.spec.alpha :as s]))

;;;; Implementation

;; fspec

(defn- ->args [ctx] (-> ctx :request :args))
(defn- ->ret [ctx] (-> ctx :response :ret))
(defn- ->fn [ctx] {:args (->args ctx) :ret (->ret ctx)})

(defn- stage-fn
  "Return a function that can be used for an interceptor stage.

  * `fspec-k`: the relevant kwarg of the `s/fspec` (:args, :ret, or :fn)

  * `data-fn`: a function to extract the relevant values from a context

  * `ctx-fn`: an optional function to apply to the resulting context before
  stage-fn returns"
  [stage fspec-k data-fn]
  (fn validate [{{:keys [sym]} :request :as ctx}]
    (if-let [ed (some-> sym s/get-spec fspec-k (s/explain-data (data-fn ctx)))]
      (let [msg (str "Call to " sym " did not conform to spec.")
            data (cond-> {:stage stage
                          :interceptor ::fspec
                          ::s/fn sym
                          ::s/failure :instrument}
                   true (into ed)
                   (identical? :args fspec-k) (assoc ::s/args (::s/value ed))
                   (identical? :ret fspec-k) (assoc ::s/ret (::s/value ed)))]
        (assoc ctx :error (ex-info msg data)))
      ctx)))

;; fdef

(defn- sym->data [sym]
  (when-let [data (meta (requiring-resolve sym))]
    (assoc data ::params (->> data :arglists (eduction (filter meta))))))

(defn- tag
  "Return a map destructring the fixed and varadic args.

  `::tag` is a string describing the params (suitible in `s/or` and `s/alt`
  forms)."
  [params]
  (let [[fixed [_ vargs]] (split-with (complement #{'&}) params)
        prefix (if vargs "variadic" "arity")
        n (count fixed)]
    {::fixed fixed
     ::vargs vargs
     ::params params
     ::tag (keyword (str prefix "-" n))}))

(defn- args-spec-form
  "Interpret different styles of declaring `:args` specs.

  `:args` value must be syntax-quoted (`).

  e.g. for params [a b c & d]:

  * Explicit clojure.spec regex: same form as you'd use in an `s/fdef` or
  `s/fspec` e.g. `(s/cat :a int? :b int? :c (s/? boolean?) :d (s/* any?))

  * Coll of predicates: predicates correspond to the position of the params.
  Handles varadic arguments for you e.g. `[int? int? (s/? boolean?) any?]

  Keys params without a named binding are generated with the following scheme: a
  keyword with a short character describing the form type (m, v, _ for maps,
  vectors, and _ respectively) followed by __ its index in the params vector
  e.g. for

  [_ {:keys [x]} [y _] & {:as opts}] with :args `[int? map? vector?],
  the :args spec generated will be:

  (s/cat :___0 int? :m__1 map? :v__2 vector? :opts (s/keys*))"
  [specs {::keys [fixed vargs params sym]}]
  (if (s/regex? specs)
    specs
    (letfn [(gen-k [index s]
              (keyword (str s "__" index)))
            (format-k [index form]
              (cond
                (= '_ form) (gen-k index "_")
                (symbol? form) (keyword form)
                (map? form) (or (format-k index (get form :as))
                                (gen-k index "m"))
                (vector? form) (if (some #{:as} form)
                                 (format-k index (peek form))
                                 (gen-k index "v"))))
            (format-vargs [index vspec vargs]
              [(format-k index vargs) (cond
                                        (s/regex? vspec) vspec
                                        (some? vspec) `(s/* ~vspec)
                                        (map? vargs) `(s/keys*)
                                        :else `(s/* any?))])]
      (let [ks (map-indexed format-k fixed)
            specs (if (coll? specs) (vec specs) [specs])
            vargs? (some? vargs)
            num-fixed (count fixed)     ; also index of vargs spec (vspec)
            num-params (cond-> num-fixed vargs? inc)
            vspec (get specs num-fixed) ; nil pun instead of nth exception
            body (cond-> (interleave ks (cond->> specs vspec pop))
                   vargs? (concat (format-vargs num-fixed vspec vargs)))]
        (when-not (<= num-fixed (count specs) num-params)
          (let [msg (str ":args metadata vector should match number forms in"
                         " the arglists (declaring variadic args is optional)")]
            (throw (ex-info  msg {:fn sym :params params :args specs}))))
        `(s/cat ~@body)))))

(defn- fspec-form [sym opts]
  (letfn [(->disjunction-sym [kwarg]
            (case kwarg :args `s/alt (:ret :fn) `s/or))
          (wrap-disjunction [tags kwarg]
            (if (= 1 (count tags))
              (first (vals tags))       ; omit tag and return unwrapped spec
              (cons (->disjunction-sym kwarg) (eduction cat tags))))
          (kwargs-tags [acc {-tag ::tag :as m}]
            (let [{:keys [args ret] f :fn} opts ; f to avoid shadowing fn
                  [oargs oret ofn] (map (fn [opt] (eval (opt m))) [args ret f])]
              (cond-> acc
                oargs (assoc-in [:args -tag] (args-spec-form oargs m))
                oret (assoc-in [:ret -tag] oret)
                ofn (assoc-in [:fn -tag] ofn))))]
    (when-let [kvs (some->> sym
                            sym->data
                            ::params
                            (transduce
                             (map (fn [params]
                                    (merge {::sym sym} ; for arg-spec errors
                                           (meta params)
                                           (tag params))))
                             (completing kwargs-tags)
                             {})
                            (eduction
                             (keep (fn [[kwarg tags]]
                                     (when (seq tags)
                                       [kwarg (wrap-disjunction tags kwarg)])))
                             cat))]
      `(s/fspec ~@kvs))))

(defn- register-fdef [sym opts]
  (eval `(s/def ~sym ~(fspec-form sym opts))))

(defn- unregister-fdef [sym]
  (eval `(s/def ~sym nil)))

;;;; API

(defn fspec
  "A function interceptor for instrumenting clojure.spec fspecs.

  Takes `opts` map which keys can include:

  * fspec keys (`:args`, `:ret`, `:fn`): When truthy, enable checking of that
  key.

  A 0-arity call defaults to `stest/instrument`'s behavior of only checking the
  :args spec."
  ([]
   (fspec {:args true}))
  ([opts]
   (cond-> {:name ::fspec}
     (:args opts) (assoc :enter (stage-fn :enter :args ->args))
     (:ret opts) (assoc :leave (stage-fn :leave :ret ->ret))
     (:fn opts) (update :leave (fn [f]
                                 (cond-> (stage-fn :leave :fn ->fn)
                                   (some? f) (comp f)))))))

(defn fdef-via-metadata
  "An instrument interceptor for registering `s/fdef`s from `defn` metadata."
  ([]
   (fdef-via-metadata {:args :args :ret :ret :fn :fn}))
  ([opts]
   {:name ::fdef
    :enter (fn enter [ctx]
             (-> ctx :request :sym (register-fdef opts))
             ctx)
    :leave (fn leave [ctx]
             (-> ctx :request :sym unregister-fdef)
             ctx)}))

(comment

  (def ^:private spec-qualified-opts
    {:args ::s/args :ret ::s/ret :fn ::s/fn})

  (s/def ::c boolean?)

  (defn- f
    [a [_ _ :as b] {:keys [c]} & {:as opts}]
    {::s/args `[int? (s/coll-of int?) (s/keys ::req-un [::c])]}
    [a b c opts])

  (= (-> `f (register-fdef spec-qualified-opts) s/get-spec s/form)
     `(s/fspec :args (s/cat :a int?
                            :b (s/coll-of int?)
                            :m__2 (s/keys ::req-un [::c])
                            :opts (s/keys*))
               :ret any?
               :fn nil))

  (unregister-fdef `f)
  (s/get-spec `f))
