(ns org.candelbio.multitool.cljcore-test
  (:require [clojure.string :as str])
  (:use clojure.test)
  (:use org.candelbio.multitool.cljcore)
  (:use org.candelbio.multitool.core))

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

(deftest pdoseq-test
  (let [acc (atom nil)]
    ;; Doing this with regular doseq would (a) produce a sorted list (b) take a lot longer
    (pdoseq [i (range 100)]
            (Thread/sleep (long (rand-int 100)))
            (swap! acc conj i))
    (Thread/sleep 200)                  
    (is (= 100 (count @acc)))           ;TODO sometimes fails
    (is (= 4950 (reduce + @acc)))
    ;; order should be scrambled (guess there is a very small chance it won't be!)
    (is (not (= (range 100) @acc)))))

(deftest pdoseq*-test
  (let [acc (atom nil)]
    (pdoseq* [i '(a b c)
              j '(x y z)]
            (Thread/sleep (long (rand-int 100)))
            (swap! acc conj (vector i j)))
    (Thread/sleep 100)
    (is (= 3 (count @acc)))
    (is (set= @acc '[[a x] [b y] [c z]]))
    ))
