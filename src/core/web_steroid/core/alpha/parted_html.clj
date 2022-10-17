(ns web-steroid.core.alpha.parted-html
  (:require
   [clojure.string :as str]
   ))


(defn lines-start-at
  [lines key]
  (->> lines
    (drop-while
      (fn [line]
        (not (re-matches (re-pattern (str "^<!-- " key " -->\\s*$")) line))))
    (rest)))


(defn get-current-part-text
  [lines]
  (->> lines
    (take-while
      (fn [line] (not (re-matches #"^<!-- :[^\s]+ -->\s*$" line))))
    (str/join "\n")))


(defn parse-parted-html
  [text keys]
  (let [tcoll (transient {})
        lines (str/split text #"\n")]
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
