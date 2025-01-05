(ns org.candelbio.multitool.browser
  (:require [org.candelbio.multitool.cljscore :as core]
            [clojure.string :as str]))

;;; ⩇⩆⩇ Browser-specific ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn browser-url
  "Get URL of current page"            
  []
  (-> js/window .-location .-href))

(defn browser-pathname               
  "Get the pathname (URL after host)"
  []
  (-> js/window .-location .-pathname))

(defn browser-path
  "Get the pathname as a sequence of components"
  []
  (str/split (subs (browser-pathname) 1) "/"))

(defn url-params
  "Get the url params from the browser window as a map"
  []
  (let [params (js/URLSearchParams. (.-search (.-location js/window)))
        names (core/iter->lseq (.keys params))]
    (zipmap (map keyword names)
            (map #(.get params %) names))))

(defn ->url-params
  "Convert param map to string suitable for inclusion in URL"
  [param-map]
  (let [params (js/URLSearchParams. (clj->js param-map))]
    (str params)))

(defn host
  "Get hostname of currnet URL"
  []
  (.-host (.-location js/window)))

(defn copy-to-clipboard
  "Copy contents of HTML element into clipboard. Kludgey."
  [id]
  (let [elt (.getElementById js/document id)]
    (.select elt)
    (.setSelectionRange elt 0 99999)
    (.execCommand js/document "copy")))

(defn open-in-browser-tab
  [url name]
  (let [win (.open js/window name)]
    (.assign (.-location win) url)))
