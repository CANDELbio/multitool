(ns org.parkerici.multitool.nlp-test
  (:use clojure.test)
  (:use org.parkerici.multitool.nlp)
  (:require [clojure.string :as str]))

(deftest token-test                     ; in more ways than one!
  (is (= ["foo" "bar"] (tokens "foo bar"))))

