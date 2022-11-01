(ns org.parkerici.multitool.cljcore-test
  (:require [clojure.string :as str])
  (:use clojure.test)
  (:use org.parkerici.multitool.cljcore))

(deftest parse-long-or-nil-test
  (is (nil? (parse-long-or-nil nil)))
  (is (= 231 (parse-long-or-nil "231")))
  (is (thrown? Exception (parse-long-or-nil 'a)) )
  (is (thrown? Exception (parse-long-or-nil "foo"))))

(deftest string-search-all-test
  (is (= '(5 17) (string-search-all "I am foolish for food" "foo"))))

(deftest substitute-file-lines-test
  (let [out (temp-file-path)]
    ((substitute-file-lines {"sunny" "weird"
                             "Kubla" "Genghis"})
     "test/resources/xanadu.txt"
     out)
    (let [out-lines (file-lines out)]
      (is (= "In Xanadu did Genghis Khan"
             (first out-lines)))
      (is (= 3 (count (filter #(str/includes? % "weird") out-lines)))))))

   
