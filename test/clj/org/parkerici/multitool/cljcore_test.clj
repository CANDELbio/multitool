(ns org.parkerici.multitool.cljcore-test
  (:use clojure.test)
  (:use org.parkerici.multitool.cljcore))

(deftest parse-long-or-nil-test
  (is (nil? (parse-long-or-nil nil)))
  (is (= 231 (parse-long-or-nil "231")))
  (is (thrown? Exception (parse-long-or-nil 'a)) )
  (is (thrown? Exception (parse-long-or-nil "foo"))))

(deftest string-search-all-test
  (is (= '(5 17) (string-search-all "I am foolish for food" "foo"))))



