(ns web-steroid.core.alpha
  (:require
   [clojure.spec.alpha :as s]
   [clojure.java.io :as jio]
   [clojure.string :as str]
   [hawk.core :as hawk]
   [user.ring.alpha :as user.ring]
   [web-steroid.core.alpha.util :as util]
   [web-steroid.core.alpha.merge :as merge]
   [web-steroid.core.alpha.parted-html :as parted-html]
   [web-steroid.core.alpha.server-render :as server-render]
   ))


(s/def :html/attrs-string string?)
(s/def :html.head/contents-string string?)
(s/def :html.body/contents-string string?)


(def ^{:dynamic true :private true} *render?*
  "predicate fn with [request] args that return boolean whether render or not"
  (constantly true))


;;


(defn auto-update-reference
  "read-fn should be wrapped with try-catch block."
  ([file-path read-fn]
   (auto-update-reference file-path read-fn {}))
  ([file-path read-fn watch-opts]
   (let [reference (atom
                     (try
                       (when-some [initial (read-fn file-path)]
                         initial)
                       (catch Exception e
                         (tap> [::auto-update-reference :exception (type e) (ex-message e)])
                         nil)))]
     (alter-meta! reference merge
       {:hawk/watch
        (hawk/watch!
          watch-opts
          [{:paths   [(str (jio/as-file file-path))]
            :handler (fn [_ctx {:keys [file]}]
                       (try
                         (when-some [new (read-fn file)]
                           (reset! reference new)
                           (tap> [::auto-update-reference :hawk/watch (str file)]))
                         (catch Exception e
                           (tap> [::auto-update-reference :hawk/watch :exception (type e) (ex-message e)]))))}])
        :file-path file-path})
     reference)))


;; * Server Render


(def ^{:arglists '([{:as params}])}
  render-html server-render/render-html)


(defn- render-anti-forgery-token!
  [attr-name anti-forgery-token sb]
  (cond
    (not (string? anti-forgery-token)) :nop
    (str/blank? anti-forgery-token)    :nop

    :else (server-render/render-meta-element! {:name attr-name :content anti-forgery-token} sb)))


;; * Ring response


(defn html-response
  [request]
  (let [charset (:html/charset request)]
    {:status  200
     :headers { "Content-Type" (str "text/html" (when charset (str "; charset=" charset))) }
     :body    (render-html request)}))


;; * path config edn


(defn wrap-path-config-edn
  [handler & [nf]]
  (user.ring/wrap-path-component
    handler
    ::path-config
    "index.edn"
    util/read-edn-file-or-resource
    nf))


;; * :html/metadata


(s/def :html.metadata.kv/key qualified-keyword?)
(s/def :html.metadata.kv.option/wrap fn?)
(s/def :html.metadata.kv.option/alts (s/coll-of :html.metadata.kv/key :kind vector?))
(s/def :html.metadata.kv/option (s/keys :opt-un [:html.metadata.kv.option/wrap :html.metadata.kv.option/alts]))
(s/def :html.metadata/kv (s/tuple :html.metadata.kv/key :html.metadata.kv/option))
(s/def :html.metadata/kvs (s/coll-of :html.metadata/kv))


(def default-html-metadata-kvs
  [[:html/title {}]
   [:html/description {:wrap (fn [v] {:name "description" :content v})}]
   [:html/viewport {:wrap (fn [v] {:name "viewport" :content v})}]
   [:html/robots {:wrap (fn [v] {:name "robots" :content v})}]
   [:html/reply-to {:wrap (fn [v] {:name "reply-to" :content v})}]
   [:og/title {:wrap (fn [v] {:property "og:title" :content v}) :alts [:html/title]}]
   [:og/description {:wrap (fn [v] {:property "og:description" :content v}) :alts [:html/description]}]
   [:og/type {:wrap (fn [v] {:property "og:type" :content v})}]
   [:og/url {:wrap (fn [v] {:property "og:url" :content v})}]
   [:og/image {:wrap (fn [v] {:property "og:image" :content v})}]
   [:og.image/alt {:wrap (fn [v] {:property "og:image:alt" :content v})}]
   [:og/site_name {:wrap (fn [v] {:property "og:site_name" :content v})}]
   [:twitter/card {:wrap (fn [v] {:name "twitter:card" :content v})}]
   [:twitter/title {:wrap (fn [v] {:name "twitter:title" :content v} :alts [:html/title :og/title])}]
   [:twitter/description {:wrap (fn [v] {:name "twitter:description" :content v}) :alts [:html/description :og/description]}]
   [:twitter/url {:wrap (fn [v] {:name "twitter:url" :content v})}]
   [:twitter/image {:wrap (fn [v] {:name "twitter:image" :content v})}]
   [:twitter.image/src {:wrap (fn [v] {:name "twitter:image:src" :content v})}]
   [:twitter.image/alt {:wrap (fn [v] {:name "twitter:image:alt" :content v})}]
   [:twitter/creator {:wrap (fn [v] {:name "twitter:creator" :content v})}]
   [:twitter/site {:wrap (fn [v] {:name "twitter:site" :content v})}]
   [:al.ios/url {:wrap (fn [v] {:property "al:ios:url" :content v})}]
   [:al.ios/app_store_id {:wrap (fn [v] {:property "al:ios:app_store_id" :content v})}]
   [:al.ios/app_name {:wrap (fn [v] {:property "al:ios:app_name" :content v})}]
   [:al.android/url {:wrap (fn [v] {:property "al:android:url" :content v})}]
   [:al.android/app_name {:wrap (fn [v] {:property "al:android:app_name" :content v})}]
   [:al.android/package {:wrap (fn [v] {:property "al:android:package" :content v})}]
   [:al.web/url {:wrap (fn [v] {:property "al:web:url" :content v}) :alts [:al.android/url]}]])


(defn wrap-html-metadata
  ([handler & {:keys [kvs search excludes]
               :or   {kvs    default-html-metadata-kvs
                      search (fn [request param-key]
                               (let [find-param (some-fn param-key)]
                                 (find-param request (::path-config request))))}}]
   {:pre [(s/valid? :html.metadata/kvs kvs)
          (fn? search)]}
   (user.ring/wrap-transform-request
     handler
     (fn [request]
       (let [map-kvs
             (into (array-map) kvs)

             metadata
             (reduce
               (fn
                 [ret [param-key {:keys [alts]}]]
                 (if (contains? excludes param-key)
                   ret
                   (let [found (or (search request param-key)
                                   (some #(get ret %) alts))]
                     (cond-> ret
                       found (assoc param-key found)))))
               (array-map)
               kvs)]
         (cond-> request
           metadata
           (assoc :html/metadata
             (reduce-kv
               (fn [ret k v]
                 (let [wrap (:wrap (get map-kvs k))]
                   (cond-> ret
                     wrap (conj (wrap v)))))
               []
               metadata))))))))


;; * parted html


(def ^{:arglists '([text keys])}
  parse-parted-html parted-html/parse-parted-html)


(def default-parted-html-keys
  [:html.head.pre/contents-string
   :html.head/contents-string
   :html.head.post/contents-string
   :html.body.pre/contents-string
   :html.body/contents-string
   :html.body.post/contents-string])


(defn- merge-parted-html
  [request parsed]
  (merge/merge-maps
    {:html.head.pre/contents-string  str
     :html.head/contents-string      str
     :html.head.post/contents-string str
     :html.body.pre/contents-string  str
     :html.body/contents-string      str
     :html.body.post/contents-string str}
    request
    parsed))


(defn wrap-path-parted-html
  ([handler]
   (wrap-path-parted-html handler nil nil))
  ([handler nf]
   (wrap-path-parted-html handler nf nil))
  ([handler nf keys]
   (let [keys (or keys default-parted-html-keys)]
     (user.ring/wrap-transform-request
       handler
       (fn [request]
         (try
           (cond
             (and (:html-request? request)
                  (::path-parted-html? request true))
             (let [component (or (user.ring/path-component request "parted.html") nf)
                   text      (slurp component)
                   parsed    (parse-parted-html text keys)]
               (cond-> request
                 (some? component) (assoc ::path-parted-html component)
                 (some? parsed)    (merge-parted-html parsed)))

             :else
             request)

           (catch Exception e
             (tap> [::wrap-path-parted-html e])
             request)))))))


;; * Ring middleware


(defn wrap-update-param
  "`update-fn` [value request]"
  [handler key update-fn]
  (user.ring/wrap-transform-request
    handler
    (fn [request]
      (update request key update-fn request))))


(defn wrap-asset
  "`reference` is atom created by `auto-update-reference`"
  [handler key update-fn reference asset-name]
  (user.ring/wrap-transform-request
    handler
    (fn [request]
      (let [{asset asset-name} @reference]
        (update request key update-fn request asset)))))


(defn custom-render-predicate
  "predicate = (fn [request])"
  [handler predicate]
  {:pre [(fn? predicate)]}
  (fn
    ([request]
     (binding [*render?* predicate]
       (handler request)))
    ([request respond raise]
     (binding [*render?* predicate]
       (handler request respond raise)))))


(defn wrap-html-render
  "
  - `key` html contents-string
  - `render!` [request sb]
  - conditional render with `*render?`
  "
  [handler key render!]
  (user.ring/wrap-transform-request
    handler
    (fn [request]
      (cond-> request
        (and (:html-request? request)
             (*render?* request))
        (update key
          (fn [compiled]
            (let [sb (StringBuilder.)]
              (render! request sb)
              (str compiled sb))))))))


(defn wrap-html-render-asset
  "
  - `reference` is atom created by `auto-update-reference`
  - `key` html contents-string
  - `render!` [request asset sb]
  "
  [handler key render! reference asset-name]
  (wrap-html-render
    handler
    key
    (fn [request sb]
      (let [{asset asset-name} @reference]
        (render! request asset sb)))))


(defn wrap-html-render-assets
  "
  - `reference` is atom created by `auto-update-reference`
  - `key` html contents-string
  - `render!` [request asset sb]
  "
  [handler key render! reference asset-names]
  (wrap-html-render
    handler
    key
    (fn [request sb]
      (run!
        (fn [asset]
          (render! request asset sb))
        (map #(get @reference %) asset-names)))))


(defn wrap-html-render-anti-forgery-token
  "edge middleware"
  [handler & {:keys [name] :or {name "csrf-token"}}]
  (wrap-html-render
    handler
    :html.head/contents-string
    (fn [{:keys [anti-forgery-token] :as request} sb]
      (render-anti-forgery-token! name anti-forgery-token sb))))
