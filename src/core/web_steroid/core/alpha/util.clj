(ns web-steroid.core.alpha.util
  (:require
   [clojure.java.io :as jio]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   )
  (:import
   java.io.PushbackReader
   ))


(defn read-edn-file-or-resource
  [readable]
  (with-open [rdr (jio/reader readable)]
    (edn/read (PushbackReader. rdr))))


(defn read-edn-file
  [edn-file]
  (with-open [rdr (jio/reader (jio/as-file edn-file))]
    (edn/read (PushbackReader. rdr))))


(defn read-json-file
  [json-file]
  (with-open [rdr (jio/reader (jio/as-file json-file))]
    (json/read rdr)))
