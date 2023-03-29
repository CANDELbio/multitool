(ns org.candelbio.multitool.clj-testing)

;;; Useful functions for writing tests

(defmacro capture-output
  "Run form, recording console output and returning it as a seq of lines."
  [form]
  `(with-in-str (with-out-str ~form)
     (doall (line-seq (java.io.BufferedReader. *in*)))))

(defn file-contents?
  [f str]
  (= str (slurp f)))


