(ns web-steroid.hooks.alpha
  (:require
   [clojure.test :refer [deftest is are]]
   [clojure.java.io :as jio]
   [web-steroid.hooks.alpha :as hooks]
   ))


(def state
  {:shadow.build/config
   {:asset-path    "/"
    :output-dir    (jio/file (System/getProperty "user.dir") "src/test/test/assets")
    :build-options {:manifest-name "manifest.json"}}})


(deftest hooks
  (hooks/print-body-script-modules
    state
    (str (jio/file (System/getProperty "java.io.tmpdir") "body_script_modules.edn"))))


(comment
  (slurp (str (jio/file (System/getProperty "java.io.tmpdir") "body_script_modules.edn")))
  )
