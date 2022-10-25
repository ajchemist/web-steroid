(ns web-steroid.core.alpha.parted-html
  (:require
   [clojure.string :as str]
   ))


(def separator-line-re #"^\s*<!-- :[^\s]+ -->\s*$")


(defn pre-part-lines
  [lines]
  (take-while
    (fn [line]
      (not (re-matches separator-line-re line)))
    lines))


(defn parse-html-attrs-string
  [lines]
  (some
    (fn [line]
      (when-some [[_ attrs-string] (re-matches #"^\s*<html\s?(.*)>\s*$" line)]
        attrs-string))
    lines))


(defn lines-start-at
  [lines key]
  (->> lines
    (drop-while
      (fn [line]
        (not (re-matches (re-pattern (str "^\\s*<!-- " key " -->\\s*$")) line))))
    (rest)))


(defn get-current-part-text
  [lines]
  (->> lines
    (take-while
      (fn [line] (not (re-matches separator-line-re line))))
    (str/join "\n")))


(defn parse-parted-html
  [text keys]
  (let [tcoll    (transient {})
        lines    (str/split text #"\n")
        pre-part (pre-part-lines lines)]
    (when-some [attrs-string (parse-html-attrs-string pre-part)]
      (assoc! tcoll :html/attrs-string attrs-string))
    (reduce
      (fn [remains key]
        (let [remains' (lines-start-at remains key)]
          (assoc! tcoll key (get-current-part-text remains'))
          ;; found part key
          ;; or restart
          (if (seq remains')
            remains'
            remains)))
      lines
      keys)
    (persistent! tcoll)))
