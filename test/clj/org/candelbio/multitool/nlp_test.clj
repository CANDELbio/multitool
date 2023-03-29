(ns org.candelbio.multitool.nlp-test
  (:use clojure.test)
  (:use org.candelbio.multitool.nlp)
  (:require [clojure.string :as str]))

(deftest token-test                     ; in more ways than one!
  (is (= ["foo" "bar"] (tokens "foo bar"))))

