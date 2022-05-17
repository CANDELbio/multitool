(ns org.parkerici.multitool.norvig
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [org.parkerici.multitool.core :as u]
            ))

;;; Conveniet access to Norvig's ngram files. 

(defn- ngrams-file
  [file]
  (str "http://norvig.com/ngrams/" file))

(defn url-lines
  [url]
  (-> url
      io/reader
      line-seq))

(defn read-freqs [file]
  (reduce
   (fn [map line]
            (let [[word count] (s/split line #"\t")
                 count (Long. count)]
              (assoc map word count)))
   {}
   (url-lines (ngrams-file file))))

(u/defn-memoized freq-table [name]
  (read-freqs name))


#_
(def word-counts (freq-table "count_1w.txt"))

#_
(def stop-words (take 30 (reverse (sort-by second word-counts))))
