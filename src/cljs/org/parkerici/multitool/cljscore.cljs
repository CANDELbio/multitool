(ns org.parkerici.multitool.cljscore)

;;; Clojurescript utilities

;;; ⩇⩆⩇ General ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Doesn't work?
(defn obj->clj
  "Convert random js object into a clj map"
  [obj]
  (if (goog.isObject obj)
    (-> (fn [result key]
          (let [v (goog.object/get obj key)]
            (if (= "function" (goog/typeOf v))
              result
              (assoc result key (obj->clj v)))))
        (reduce {} (.getKeys goog/object obj)))
    obj))

;;; Nonrecursive version
(defn obj->clj-nr
  "Convert random js object into a clj map"
  [obj]
  (if (goog.isObject obj)
    (-> (fn [result key]
          (let [v (goog.object/get obj key)]
            (if (= "function" (goog/typeOf v))
              result
              (assoc result key v))))
        (reduce {} (.getKeys goog/object obj)))
    obj))



;;; ⩇⩆⩇ Browser-specific ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; TODO why this way?
(def iter->lseq
  (comp
   (fn consume [iter]
      (as-> (.next iter) step
        (when (false? (.-done step))
          (lazy-seq (cons (js->clj (.-value step))
                          (consume iter))))))
    #(js-invoke % js/Symbol.iterator)))

(defn url-params
  "Get the url params from the browser window as a map"
  []
  (let [params (js/URLSearchParams. (.-search (.-location js/window)))
        names (iter->lseq (.keys params))]
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
