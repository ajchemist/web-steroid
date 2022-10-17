(ns web-steroid.core.alpha.integrant
  (:require
   [hawk.core :as hawk]
   [integrant.core :as ig]
   [web-steroid.core.alpha :as web-steroid]
   [web-steroid.core.alpha.util :as web-steroid.util]
   ))


(defmethod ig/init-key ::auto-update-reference
  [_ {:keys [file-path read-fn watch-opts]}]
  (web-steroid/auto-update-reference file-path read-fn watch-opts))


(defmethod ig/halt-key! ::auto-update-reference
  [_ a]
  (when-some [w (:hawk/watch (meta a))]
    (println "[hawk/stop!]:" (:file-path (meta a)))
    (hawk/stop! w)))


(derive ::webpack-asset-manifest-reference ::auto-update-reference)
(derive ::body-script-modules-reference ::auto-update-reference)


(defmethod ig/prep-key ::webpack-asset-manifest-reference
  [_ config]
  (assoc config :read-fn web-steroid.util/read-json-file))


(defmethod ig/prep-key ::body-script-modules-reference
  [_ config]
  (assoc config :read-fn web-steroid.util/read-edn-file))
