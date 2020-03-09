(ns multitool.wordstats-test
  (:use clojure.test)
  (:use multitool.wordstats)
  (:require [clojure.string :as str]))


(deftest token-test                     ; in more ways than one!
  (is (= ["foo" "bar"] (tokens "foo  bar")))
  )
