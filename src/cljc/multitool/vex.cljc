(ns multitool.vex)

;;; Vectorized fns (after SciCL)

;;; Given a fn f with scalar args, (vectorized f) takes either scalars or vectors for any argument,
;;; doing the appropriate vectorization.
;;; All vector args have to be the same length.

(defn vectorize
  [f]
  (fn [& args]
    (if (some sequential? args)
      (let 
        (map (fn [i] (apply f     ))
             (range l)))
      (apply f args))))
