(ns web-steroid.rum.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [rum.core :as rum]
   [web-steroid.rum.alpha :as web.rum]
   [reitit.ring]
   ))


(rum/defc header
  []
  [:.header])


(rum/defc home
  []
  [:.home])


(def ring-handler
  (reitit.ring/ring-handler
    (reitit.ring/router
      [["/"
        {:html-request? true
         :get
         (fn [request]
           (web.rum/html-response
             [:<>
              [:header (web.rum/->RootComponent (header))]
              [:main (web.rum/->RootComponent (home))]]
             request))}]])))


(deftest main
  (is (= (ring-handler {:uri "/" :request-method :get})
         {:status  200,
          :headers {"Content-Type" "text/html"},
          :body    "<!doctype html><html><head></head><body><header><div class=\"header\" data-reactroot=\"\"></div></header><main><div class=\"home\" data-reactroot=\"\"></div></main></body></html>"}))
  )
