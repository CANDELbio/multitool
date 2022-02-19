(ns org.parkerici.multitool.browser
  (:require [org.parkerici.multitool.cljscore :as core]))

;;; ⩇⩆⩇ Browser-specific ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn browser-url
  ":Get URL of current page"            
  []
  (-> js/window .-location .-href))

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
