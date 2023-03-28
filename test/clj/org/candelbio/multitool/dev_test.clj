(ns org.candelbio.multitool.dev-test
  (:use clojure.test)
  (:use org.candelbio.multitool.dev)
  (:require [org.candelbio.multitool.cljcore :as ju]
            [org.candelbio.multitool.clj-testing :as testing]
            ))

(deftest capture-to-file-test
  (let [file (ju/temp-file)]
    (capture-to-file
     #(do (prn :x) (prn :y))
     file)
    (is (testing/file-contents? file ":x\n:y\n"))))
  
