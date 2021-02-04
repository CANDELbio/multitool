(ns org.parkerici.multitool.core-test
  (:use clojure.test)
  (:use org.parkerici.multitool.core)
  (:require [clojure.string :as str]))

(deftest underscore->camelcase-test
  (is (= (underscore->camelcase "foo_bar") "fooBar"))
  (is (= (underscore->camelcase "foo") "foo")))

(deftest map-invert-multiple-test
  (is (= {} (map-invert-multiple {})))
  (is (= '{1 #{:a}} (map-invert-multiple {:a 1})))
  (is (= '{1 #{:a} :x #{:b} :y #{:b}} (map-invert-multiple {:a 1 :b '(:x :y)})))
  (is (= '{1 #{:a :b} 2 #{:b} } (map-invert-multiple {:a 1 :b '(1 2)})))
  )

(deftest map-chunked-test
  (let [f (fn [x] (* x 2))]
    (= (map f (range 100))
       (map-chunked #(map f %) 7 (range 100)))))
      
(deftest something-test
  (is (= 2 (something even? '(1 2 3 4)))))

(deftest repeat-until-test
  (is (= 16 (repeat-until #(> % 10) #(* % 2) 1))))

(deftest positions-test
  (is (= '(0 2 4 6 8) (positions even? '(0 1 2 3 4 3 2 1 0))))
  (is (= '(3 5) (positions= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest position-test
  (is (= 1 (position even? '(1 2 3 4 3 2 1 0))))
  (is (= nil (position string? '(1 2 3 4 3 2 1 0))))
  (is (= 3 (position= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest powerset-test

  (is (= #{#{} #{3} #{2} #{1} #{1 3 2} #{1 3} #{1 2} #{3 2}}
         (powerset #{1 2 3})))
  (testing "works on empty set"
    (is (= #{#{}} (powerset #{}))))
  (testing "works on lists"
    (is (= '#{#{a} #{c b} #{a b} #{} #{b} #{c} #{a c} #{a c b}}
           (powerset '(a b c)))))
  (testing "works on decent sized list"
    (is (= (Math/round (Math/pow 2 18))
           (count (powerset (range 18)))))))

(deftest transitive-closure-test
  (let [tree [:organisms
              [:plants [:trees] [:cacti]]
              [:animals
               [:mammals [:elephants] [:cats]]
               [:birds [:terns] [:warblers] [:owls]]]]
        children #(subvec % 1)
        descendents (transitive-closure children)]
    (is (= (set (map first (children tree)))
           #{:plants :animals}))
    (is (= (set (map first (descendents tree)))
           #{:terns :cats :birds :elephants :plants :warblers :owls
             :trees :animals :organisms :mammals :cacti}))))

(deftest compare-tests
  (is (>* 2 1))
  (is (>* "foo" "bar"))
  (is (not (>* 1 1)))
  (is (<* 2 3))
  (is (<* "foo" "quux"))
  (is (not (<* 1 1))))

(defn tokens [s]
  (str/split s #"\W"))

(deftest maxby-minby-test
  (is (= "tediously"
         (max-by count (tokens "this is a tediously long string"))))
  (is (= "a"
         (min-by count (tokens "this is a tediously long string"))))
  (is (nil? (max-by count [])))
  (is (nil? (min-by count []))))

(defn rcons [a b] (cons b a))

(deftest doseq*-test
  (let [acc (atom '())]
    (doseq* [a '(1 2 3)
             b '(x y z)]
            (swap! acc rcons (list a b)))
    (= '((1 x) (2 y) (3 z))
       (reverse @acc)))
  ;; TODO seqs of different length
  )

(deftest expand-template-string-test
  (let [template "The {foo} must have {bar}!"
        ent1 {"foo" "subgenius" "bar" "slack"}
        ent2 {"foo" "dog"}]
    (is (= "The subgenius must have slack!"
           (expand-template-string template ent1)))
    (is (= "The dog must have !"
           (expand-template-string template ent2)))))

(deftest uncollide-test
  (is (= '(1 2 3) (uncollide '(1 2 3))))
  (is (= '(1 2 4 3) (uncollide '(1 2 2 3) :new-key-fn #(* % 2))))
  (is (= '("a1" "a2-1") (uncollide '("a1" "a2") :existing '("a2") :new-key-fn #(str % "-1"))))
  (is (= '("a-1-1" "b") (uncollide '("a" "b") :existing '("a" "a-1") :new-key-fn #(str % "-1"))))
  )

(deftest ignore-errors-normal-test
  (is (= 7 (ignore-errors (+ 3 4)))))

(deftest ignore-errors-rror-test
  (is (= nil (ignore-errors (/ 0 0) (+ 3 4)))))

(deftest error-handling-fn-test
  (let [ed (error-handling-fn /)]
    (is (= '(true 2/3) (ed 2 3)))
    (is (= '(false "Caught exception: java.lang.ArithmeticException: Divide by zero") (ed 2 0)))))

(deftest vectorize-test
  (let [+* (vectorize +)]
    (is (= (+* 1 2 3) 6))
    (is (= (+* 1 [4 5 6] 3)
           [8 9 10]))
    (is (= (+* [10 20 30] [1 2 3])
           [11 22 33]))))

(deftest for*-test
  (is (= '([1 a] [2 b] [3 c])
         (for* [a '(1 2 3)
                b '(a b c)]
               [a b]))))
        
(deftest re-substitute-test
  ;; Italicize all words that contain "oo"
  (is (= '("I like " [:i "food"] " and " [:i "goofing"] " on " [:i "woo"] ".")
         (re-substitute #"\w*oo\w*" "I like food and goofing on woo." (fn [ss] [:i ss])))))

(deftest index-by-test
  (is (= '{a [a 1], b [b 2], c [c 3]}
         (index-by first '[[a 1] [b 2] [c 3]]))))

(deftest coerce-numeric-test
  (is (nil? (coerce-numeric nil)))
  (is (= 23 (coerce-numeric 23)))
  (is (= 23 (coerce-numeric "23")))
  (is (= "foo" (coerce-numeric "foo")))
  (is (= "" (coerce-numeric "")))
  (is (= + (coerce-numeric +)))
  )

(deftest walk-collect-test
  (is (= [1 2 3]
         (walk-collect (or-nil number?) {:a 1 :b [2 3]}))))
