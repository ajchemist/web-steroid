(ns web-steroid.core.alpha.shadow-cljs-test
  (:require
   [clojure.java.io :as jio]
   [clojure.test :as test :refer [deftest is are testing]]
   [web-steroid.core.alpha.shadow-cljs :as web-steroid.shadow-cljs]
   ))


(def modules-config
  (web-steroid.shadow-cljs/read-manifest-json
    (jio/resource "test/assets/manifest.json")))


(deftest body-script-modules
  (is
    (= (-> modules-config
         (web-steroid.shadow-cljs/transitive-modules)
         (web-steroid.shadow-cljs/transitive-body-script-modules))

       {"libs" [{:output-name "libs.js"}], "main" [{:output-name "libs.js"} {:output-name "main.js"}]})))
