(ns web-steroid.rum.alpha
  (:require
   [web-steroid.core.alpha :as web]
   [rum.server-render :as rum.sr]
   ))


(deftype RootComponent [c]
  rum.sr/HtmlRenderer
  (-render-html [_this _*state sb]
    (rum.sr/append! sb (rum.sr/render-html c))))


(defmethod print-method RootComponent [^RootComponent component ^java.io.Writer w]
  (.write w "#web.rum/RootComponent ")
  (binding [*out* w]
    (pr (rum.sr/render-html component))))


(defn render-html
  [src request]
  (let [params (update request :html.body/contents-string
                 (fn [compiled]
                   (str compiled (rum.sr/render-static-markup src))))]
    (web/render-html params)))


(defn html-response
  [src request]
  (-> request
    (web/empty-html-response)
    (assoc :body (render-html src request))))
