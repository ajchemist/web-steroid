(ns web-steroid.core.alpha.server-render
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   ))


(def ^:const +doctype+ "<!doctype html>")
(def ^:const +doctype-html-4-01+ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
(def ^:const +doctype-xhtml-1-1+ "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">")


(defn append!
  ([^StringBuilder sb s0]
   (.append sb s0))
  ([^StringBuilder sb s0 s1]
   (.append sb s0)
   (.append sb s1))
  ([^StringBuilder sb s0 s1 s2]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2))
  ([^StringBuilder sb s0 s1 s2 s3]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2)
   (.append sb s3))
  ([^StringBuilder sb s0 s1 s2 s3 s4]
   (.append sb s0)
   (.append sb s1)
   (.append sb s2)
   (.append sb s3)
   (.append sb s4)))


(defn escape-html [^String s]
  (let [len (count s)]
    (loop [^StringBuilder sb nil
           i                 (int 0)]
      (if (< i len)
        (let [char (.charAt s i)
              repl (case char
                     \& "&amp;"
                     \< "&lt;"
                     \> "&gt;"
                     \" "&quot;"
                     \' "&#x27;"
                     nil)]
          (if (nil? repl)
            (if (nil? sb)
              (recur nil (inc i))
              (recur (doto sb
                       (.append char))
                     (inc i)))
            (if (nil? sb)
              (recur (doto (StringBuilder.)
                       (.append s 0 i)
                       (.append repl))
                     (inc i))
              (recur (doto sb
                       (.append repl))
                     (inc i)))))
        (if (nil? sb) s (str sb))))))


(defn render-doctype!
  [doctype sb]
  (append!
    sb
    (case doctype
      :html-4-01 +doctype-html-4-01+
      :xhtml-1-1 +doctype-xhtml-1-1+
      (cond
        (string? doctype) doctype
        :else             +doctype+))))


(defn render-attrs-string!
  [x sb]
  (when (string? x)
    (append! sb (str " " x))))


(defn render-contents-string!
  [x sb]
  (when (string? x)
    (append! sb x)))


(defn render-attr!
  [attr value sb]
  (when (or (string? attr)
            (keyword? attr))
    (let [attr (name attr)]
      (cond
        (not value)   :nop
        (true? value) (append! sb " " attr)
        :else         (append! sb (str " " attr "=\"" (escape-html value) "\""))))))


(defn render-attrs!
  [attrs sb]
  (reduce-kv (fn [_ k v] (render-attr! k v sb)) nil attrs))


(defn render-file!
  ([file sb]
   (render-file! file "" "" sb))
  ([file start end sb]
   (try
     (let [f (jio/as-file file)]
       (when (.isFile f)
         (when-some [contents (slurp f)]
           (append! sb start)
           (append! sb contents)
           (append! sb end))))
     (catch Exception _))))


(defn render-readable!
  ([readable sb]
   (render-readable! readable "" "" sb))
  ([readable start end sb]
   (try
     (when-some [contents (slurp readable)]
       (append! sb start)
       (append! sb contents)
       (append! sb end))
     (catch Exception _))))


(defn render-element!
  [tag attrs contents sb]
  (append! sb (str "<" tag))
  (render-attrs! attrs sb)
  (append! sb ">")
  (render-contents-string! contents sb)
  (append! sb (str "</" tag ">")))


(defn render-non-empty-element!
  [tag attrs contents sb]
  (when (string? contents)
    (render-element! tag attrs contents sb)))


(defn render-line-element!
  [attrs start end sb]
  (when (seq attrs)
    (append! sb start)
    (render-attrs! attrs sb)
    (append! sb end)))


(defn render-meta-element!
  ([x sb]
   (cond
     (vector? x) (render-meta-element! (name (first x)) (second x) sb)
     (map? x)    (render-meta-element! "meta" x sb)))
  ([tag attrs sb]
   (render-line-element! attrs (str "<" tag) ">" sb)))


(defn render-link-element!
  [attrs sb]
  (render-line-element! attrs "<link" ">" sb))


(defn render-script-element!
  [attrs sb]
  (render-line-element! attrs "<script" "></script>" sb))


(fn [{:keys [:html/metatags]} {:keys [property]}]
  (get metatags property))


(defn render-html
  [{:as params}]
  (let [sb (StringBuilder.)]
    (render-doctype! (:html/doctype params) sb)
    (append! sb "<html")
    (render-attrs-string! (:html/attrs-string params) sb)
    (append! sb ">")
    (append! sb "<head>")
    (render-non-empty-element! "title" nil (:html/title params) sb)
    (render-meta-element! (when-some [charset (:html/charset params)] {:charset charset}) sb)
    (run! (fn [meta] (render-meta-element! meta sb)) (:html/metadata params))
    (render-contents-string! (:html.head.pre/contents-string params) sb)
    (render-contents-string! (:html.head/contents-string params) sb)
    (render-contents-string! (:html.head.post/contents-string params) sb)
    (append! sb "</head>")
    (append! sb "<body>")
    (render-contents-string! (:html.body.pre/contents-string params) sb)
    (render-contents-string! (:html.body/contents-string params) sb)
    (render-contents-string! (:html.body.post/contents-string params) sb)
    (append! sb "</body>")
    (append! sb "</html>")
    (str sb)))
