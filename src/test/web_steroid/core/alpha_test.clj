(ns web-steroid.core.alpha-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is are testing]]
   [user.ring.alpha :as user.ring]
   [web-steroid.core.alpha :as web-steroid]
   [web-steroid.core.alpha.server-render :as web-steroid.sr]
   [clojure.java.io :as jio]
   ;; [reitit.core :as reitit]
   [ring.util.response :as response]
   [reitit.ring]
   [web-steroid.core.alpha.util :as util]
   ))


(def wrap-request-from-match-data
  {:name ::wrap-request-from-match-data
   :compile
   (fn [_data _opts]
     (fn [handler keys]
       (user.ring/wrap-transform-request
         handler
         (fn [request]
           (let [{:keys [data]} (reitit.ring/get-match request)]
             (reduce-kv
               (fn [ret k v]
                 (cond-> ret
                   (some? v) (assoc k v)))
               request
               (select-keys data keys)))))))})


(deftest render-html
  (is (= (web-steroid/render-html {:html/attrs-string "amp lang=kr"})
         "<!doctype html><html amp lang=kr><head></head><body></body></html>"))
  (is (= (web-steroid/render-html {:html/charset "utf-8"})
         "<!doctype html><html><head><meta charset=\"utf-8\"></head><body></body></html>"))
  )


(deftest html-response
  (is (= (web-steroid/html-response {})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head></head><body></body></html>"}))
  (is (= (web-steroid/html-response {:html/charset "utf-8"})
         {:status  200
          :headers {"Content-Type" "text/html; charset=utf-8"}
          :body    "<!doctype html><html><head><meta charset=\"utf-8\"></head><body></body></html>"}))
  )


(deftest anti-forgery-token
  (is (= ((-> (fn [request]
                (web-steroid/html-response request))
            (web-steroid/wrap-html-render-anti-forgery-token))
          {:anti-forgery-token "TEST"})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head></head><body></body></html>"}))
  (is (= ((-> (fn [request]
                (web-steroid/html-response request))
            (web-steroid/wrap-html-render-anti-forgery-token))
          {:html-request?      true
           :anti-forgery-token "TEST"})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head><meta name=\"csrf-token\" content=\"TEST\"></head><body></body></html>"})
      ":html-request? true => render anti-forgery-token")
  (is (= ((-> (fn [request]
                (web-steroid/html-response request))
            (web-steroid/wrap-html-render-anti-forgery-token)
            (web-steroid/custom-render-predicate (constantly false)))
          {:html-request?      true
           :anti-forgery-token "TEST"})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head></head><body></body></html>"}))
  (is (= ((-> (fn [request]
                (web-steroid/html-response request))
            (web-steroid/wrap-html-render-anti-forgery-token)
            (web-steroid/custom-render-predicate (constantly true))
            (web-steroid/custom-render-predicate (constantly false)))
          {:html-request?      true
           :anti-forgery-token "TEST"})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head><meta name=\"csrf-token\" content=\"TEST\"></head><body></body></html>"})))


(def handler--html-metadata
  (reitit.ring/ring-handler
    (reitit.ring/router
      [["/"
        {:get        (fn [request] (web-steroid/html-response request))
         :middleware [web-steroid/wrap-html-metadata]}]
       ["/excludes"
        {:get        (fn [request] (web-steroid/html-response request))
         :middleware [[web-steroid/wrap-html-metadata {:excludes #{:og/description :twitter/description}}]]}]]
      {:data
       {:html-request? true

        ::user.ring/path-component? true

        :middleware
        [[wrap-request-from-match-data [:html-request? ::user.ring/path-component?]]
         [user.ring/wrap-context-dir "src/test/test/assets"]
         [web-steroid/wrap-path-config-edn]]}})))


(deftest html-metadata
  (is (= (handler--html-metadata
           {:uri "/" :request-method :get})
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body "<!doctype html><html><head><meta name=\"description\" content=\"This description is from path-config index.edn\"><meta property=\"og:description\" content=\"This description is from path-config index.edn\"><meta name=\"twitter:description\" content=\"This description is from path-config index.edn\"></head><body></body></html>"}))
  (is (= (handler--html-metadata
           {:uri "/excludes" :request-method :get})
         {:status 200
          :headers {"Content-Type" "text/html"}
          :body "<!doctype html><html><head><meta name=\"description\" content=\"This description is from path-config index.edn\"></head><body></body></html>"})))


(def handler--path-parted-html
  (reitit.ring/ring-handler
    (reitit.ring/router
      [["/"
        {:get (fn [request] (web-steroid/html-response request))}]
       ["/parted-html/:path"
        {:get (fn [{:keys [path-params] :as request}]
                (response/response
                  (when-some [readable (user.ring/file-or-resource (:path path-params))]
                    (slurp readable))))}]
       ["/no-html-request"
        {:html-request? false

         :get (fn [request] (web-steroid/html-response request))}]
       ["/no-parted-html"
        {::web-steroid/path-parted-html? false

         :get (fn [request] (web-steroid/html-response request))}]
       ["/example"
        {:get (fn [request] (web-steroid/html-response request))}]
       ["/empty/"
        {:get (fn [request] (web-steroid/html-response request))}]
       ["/not-found/"
        {:get (fn [request] (web-steroid/html-response request))}]]
      {:data
       {:html-request? true

        :middleware
        [[wrap-request-from-match-data [:html-request? ::web-steroid/path-parted-html?]]
         [user.ring/wrap-context-dir "src/test/test/assets"]
         [web-steroid/wrap-path-parted-html (jio/resource "test/assets/nf.parted.html")]]}})))


(deftest path-parted-html
  (is (nil? (handler--path-parted-html {:uri "/none" :request-method :get})))
  (is (= (handler--path-parted-html {:uri "/" :request-method :get})
         {:status  200,
          :headers {"Content-Type" "text/html"},
          :body    "<!doctype html><html><head><meta name=\"description\" content=\"sample\"></head><body><p>Hello, World!</p></body></html>"}))
  (is (string? (:body (handler--path-parted-html {:uri "/parted-html/no-html-request.parted.html" :request-method :get}))))
  (is (= (handler--path-parted-html {:uri "/no-html-request" :request-method :get})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head></head><body></body></html>"}))
  (is (string? (:body (handler--path-parted-html {:uri "/parted-html/no-parted-html.parted.html" :request-method :get}))))
  (is (= (handler--path-parted-html {:uri "/no-parted-html" :request-method :get})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head></head><body></body></html>"})
      "Disabled parted-html")

  (is (= (handler--path-parted-html {:uri "/example" :request-method :get})
         {:status  200
          :headers {"Content-Type" "text/html"}
          :body    "<!doctype html><html><head><meta name=\"description\" content=\"This is example parted html\"></head><body></body></html>"})
      "not slash ended path-parted-html")
  (is (= (handler--path-parted-html {:uri "/empty/" :request-method :get})
         {:status  200,
          :headers {"Content-Type" "text/html"},
          :body    "<!doctype html><html><head></head><body></body></html>"}))
  (is (= (handler--path-parted-html {:uri "/not-found/" :request-method :get})
         {:status  200,
          :headers {"Content-Type" "text/html"},
          :body    "<!doctype html><html><head><meta name=\"description\" content=\"This is default parted html\"></head><body></body></html>"})))


(defn inherited-render?
  [request]
  (-> request (reitit.ring/get-match) (get-in [:data ::inherited-render?] true)))


(def handler-02
  (reitit.ring/ring-handler
    (reitit.ring/router
      [["/"
        {:get (fn [request] (web-steroid/html-response request))}]
       ["/disabled"
        {::inherited-render? false

         :get (fn [request] (web-steroid/html-response request))}]]
      {:data
       {:html-request? true

        :middleware
        [[wrap-request-from-match-data [:html-request?]]
         [web-steroid/custom-render-predicate inherited-render?]
         [web-steroid/wrap-html-render :html.head/contents-string
          (fn [_request sb]
            (web-steroid.sr/render-non-empty-element! "title" {} "타이틀" sb))]]}})))


(deftest html-render
  (is (= (handler-02 {:uri "/" :request-method :get})
         {:status 200, :headers {"Content-Type" "text/html"}, :body "<!doctype html><html><head><title>타이틀</title></head><body></body></html>"}))
  (is (= (handler-02 {:uri "/disabled" :request-method :get})
         {:status 200, :headers {"Content-Type" "text/html"}, :body "<!doctype html><html><head></head><body></body></html>"}))
  )


(reitit.ring/ring-handler
    (reitit.ring/router
      [["/"
        {:get (fn [request] (web-steroid/html-response request))}]
       ["/disabled"
        {::inherited-render? false

         :get (fn [request] (web-steroid/html-response request))}]]
      {:data
       {:middleware
        [[user.ring/wrap-context-dir "src/test/test/assets"]
         [web-steroid/custom-render-predicate inherited-render?]
         [web-steroid/wrap-html-render :html.head/contents-string
          (fn [_request sb]
            (web-steroid.sr/render-non-empty-element! "title" {} "타이틀" sb))]]}}))



(into []
  (fn [rf]
    (let [seen (volatile! #{})]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]

         (if (contains? @seen input)
           result
           (do (vswap! seen conj input)
               (rf result input)))))))
  [{:name "description" :content "First description"}
   {:name "description" :content "Second description"}
   {:name "description" :content "Third description"}
   {:property "description" :content "Third description with attribute name \"property\""}
   {:property "og:image" :content "https://picsum.photos/id/237/200/300"}])
