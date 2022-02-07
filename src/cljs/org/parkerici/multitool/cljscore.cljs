(ns org.parkerici.multitool.cljscore)

;;; Clojurescript utilities (note: these are mostly crap)

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

;;; TODO why this way?
(def iter->lseq
  (comp
   (fn consume [iter]
      (as-> (.next iter) step
        (when (false? (.-done step))
          (lazy-seq (cons (js->clj (.-value step))
                          (consume iter))))))
    #(js-invoke % js/Symbol.iterator)))
