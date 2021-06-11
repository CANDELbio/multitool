(ns org.parkerici.multitool.dev-test
  (:use clojure.test)
  (:use org.parkerici.multitool.dev)
  (:require [org.parkerici.multitool.cljcore :as ju]
            [org.parkerici.multitool.clj-testing :as testing]
            ))

(deftest capture-to-file-test
  (let [file (ju/temp-file)]
    (capture-to-file
     #(do (prn :x) (prn :y))
     file)
    (is (testing/file-contents? file ":x\n:y\n"))))
  
