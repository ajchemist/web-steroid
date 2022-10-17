(ns web-steroid.core.alpha.parted-html-test
  (:require
   [clojure.test :as test :refer [deftest is are]]
   [clojure.java.io :as jio]
   [web-steroid.core.alpha.parted-html :as parted-html]
   ))


(def text (slurp (jio/resource "test/assets/parted.html")))
(def text-01 (slurp (jio/resource "test/assets/example.parted.html")))
(def empty-text (slurp (jio/resource "test/assets/empty/parted.html")))


(deftest parse
  (is (= (parted-html/parse-parted-html "" [:a :b :c])
         {:a "" :b "" :c ""}))

  (is (= (parted-html/parse-parted-html
           text
           [:html.head.pre/contents-string
            :html.head/contents-string
            :html.head.post/contents-string
            :html.body.pre/contents-string
            :html.body/contents-string
            :html.body.post/contents-string])

         {:html.head.pre/contents-string  "<meta name=\"description\" content=\"sample\">"
          :html.head/contents-string      ""
          :html.head.post/contents-string ""
          :html.body.pre/contents-string  ""
          :html.body/contents-string      "<p>Hello, World!</p>"
          :html.body.post/contents-string ""}))

  (is (= (parted-html/parse-parted-html
           text-01
           [:html.head.pre/contents-string
            :html.head/contents-string
            :html.head.post/contents-string
            :html.body.pre/contents-string
            :html.body/contents-string
            :html.body.post/contents-string])

         {:html.head.pre/contents-string  "",
          :html.head/contents-string      "<meta name=\"description\" content=\"This is example parted html\">",
          :html.head.post/contents-string "",
          :html.body.pre/contents-string  "",
          :html.body/contents-string      "",
          :html.body.post/contents-string ""})))
