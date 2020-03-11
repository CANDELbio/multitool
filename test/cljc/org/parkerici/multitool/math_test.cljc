(ns org.parkerici.multitool.math-test
  (:require [org.parkerici.multitool.math :refer :all]
            #?(:clj [clojure.test :refer :all]
               :cljs [cljs.test :as t :include-macros true])))

(deftest interpolate-test
  (is (= 5.0 (interpolate 0 10 0.5)))
  (is (= 1.0 (interpolate 0 10 0.1))))

(deftest interpolated-test
  (is (= '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0) (interpolated 0 10 10))))

(deftest rescale-test
  (is (= 12.0 (rescale 0.2 0 1 10 20))))

(deftest primes-test
  (is (= '(2 3 5 7 11 13 17 19 23 29)
         (take 10 primes))))

(deftest prime-factor-test
  (is (= '(9091 11) (prime-factors 100001)))
  (testing "random numbers"
    (doseq [i (range 10)]
      (let [j (rand-int 1000000)]
        (is (= j (apply * (prime-factors j))))))))

