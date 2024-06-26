(ns org.candelbio.multitool.core-test
  (:use clojure.test)
  (:use org.candelbio.multitool.core)
  (:require [clojure.string :as str]
            [org.candelbio.multitool.nlp :as nlp]
            [org.candelbio.multitool.math :as math]))

(deftest truncate-string-test
  (is (= "foo" (truncate-string 3 "foo")))
  (is (= "fo…" (truncate-string 2 "foo")))
  (is (= "fo" (truncate-string 3 "fo"))))

(deftest memoize-named-test
  (let [counter (atom 0)
        next #(swap! counter inc)
        mem-next (memoize-named :hey next)]
    (is (not (= (next) (next))))
    (is (= (mem-next) (mem-next)))
    (let [a (mem-next)]
      (memoize-reset! :hey)
      (is (not (= (mem-next) a))))))

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
      
(deftest following-elt-test
  (is (= 3 (following-elt 2 [1 2 3 4])))
  (is (nil? (following-elt 20 [1 2 3 4])))
  (is (nil? (following-elt 4 [1 2 3 4])))
  (is (nil? (following-elt 1 [])))
  (testing "stack safety"
    (is (= 90001 (following-elt 90000 (range 100000))))))

(deftest preceding-elt-test
  (is (= 3 (preceding-elt 4 [1 2 3 4])))
  (is (nil? (preceding-elt 20 [1 2 3 4])))
  (is (nil? (preceding-elt 1 [1 2 3 4])))
  (is (nil? (preceding-elt 1 [])))
  (testing "stack safety"
    (is (= 89999 (preceding-elt 90000 (range 100000))))))

(deftest some-thing-test
  (is (= 2 (some-thing even? '(1 2 3 4)))))

(deftest repeat-until-test
  (is (= 16 (repeat-until #(> % 10) #(* % 2) 1))))

(deftest remove=-test
  (is (= '(0 1 3 4) (remove= 2 (range 5))))
  (is (= '(0 2 3 4) (remove= 2 (range 5) #(* % 2)))))

(deftest delete-subseq-test
  (let [seq '(a b c d e f)]
    (= '(a e f g) (delete-subseq seq '(b c c)))
    (= '(c d e f g) (delete-subseq seq '(a b)))
    (= '(a b c d e) (delete-subseq seq '(f g)))
    (= seq (delete-subseq seq '(a x)))
    (= '() (delete-subseq seq seq))))

(deftest positions-test
  (is (= '(0 2 4 6 8) (positions even? '(0 1 2 3 4 3 2 1 0))))
  (is (= '(3 5) (positions= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest position-test
  (is (= 1 (position even? '(1 2 3 4 3 2 1 0))))
  (is (= nil (position string? '(1 2 3 4 3 2 1 0))))
  (is (= 3 (position= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest sort-with-numeric-prefix-test
  (is (= '("foo" "0001 foo" "2 foo" "100 demons")
         (sort-with-numeric-prefix ["100 demons" "0001 foo" "2 foo" "foo"]))))

(deftest subseqs-test
  (let [seq5 '(a b c d e)]
    (is (= '((a b c) (b c d) (c d e)) (subseqs seq5 3)))
    (is (= '((a b c d) (b c d e)) (subseqs seq5 4)))
    (is (= '() (subseqs seq5 10)))))

(deftest set=-test
  (is (set= '(a b c) '(c b a)))
  (is (not (set= '(a b c) '(c b d))))
  (is (set= '(a b c) '(c b a c))))

(deftest bag=-test
  (is (bag= '(a b c) '(c b a)))
  (is (not (bag= '(a b c) '(c b d))))
  (is (not (bag= '(a b c) '(c b a c))))
  (is (bag= '(a c b c) '(c b a c))))

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

(deftest divide-with-test
  (= [[0 2 4 6 8] [1 3 5 7 9]]
     (divide-with even? (range 10))))

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

(deftest min*-max*-test
  (let [words '("you" "call" "this" "living")]
    (is (= "call" (min* words)))
    (is (= "you" (max* words)))))

(deftest doseq*-test
  (let [acc (atom '())]
    (doseq* [a '(1 2 3)
             b '(x y z)]
            (swap! acc conj (list a b)))
    (= '((1 x) (2 y) (3 z))
       (reverse @acc)))
  ;; TODO seqs of different length
  )

(deftest re-seq-positions-test
  (is (= [[7 10] [13 16] [28 31]]
         (re-seq-positions #"foo" "I like food, fooseball, and foolishness.")))
  (is (= [[6 9] [15 16] [29 34]]
         (re-seq-positions #"\((-*)\)" "This (---) is (-) something (-----) else" 1)))
  (is (= []
         (re-seq-positions #"\((-*)\)" "nada"))))

;;; TODO would make sense to have a re-seq variant that could return groups

(deftest expand-template-test
  (let [template "The {foo} must have {bar}!"
        bindings1 {"foo" "subgenius" "bar" "slack"}
        bindings2 {"foo" "dog"}]
    (is (= "The subgenius must have slack!"
           (expand-template template bindings1)))
    (is (= "The dog must have !"
           (expand-template template bindings2))))
  (testing "Double braces"
    (let [template "The {{foo}} must have {{bar}}!"
          bindings1 {"foo" "subgenius" "bar" "slack"}
          bindings2 {"foo" "dog"}]
      (is (= "The subgenius must have slack!"
             (expand-template template bindings1 :param-regex double-braces)))
      (is (= "The dog must have !"
             (expand-template template bindings2 :param-regex double-braces))))
    )
  (testing "Javascript templating, keywords"
    (let [template "The ${foo} must have ${bar}!"
          bindings1 {:foo "subgenius" :bar "slack"}]
      (is (= "The subgenius must have slack!"
             (expand-template template bindings1 :param-regex javascript-templating :key-fn keyword)))
      ))
  )

(deftest pattern-match-test
  (testing "basics"
    (is (= {} (pattern-match '(a 1) '(a 1))))
    (is (= nil (pattern-match '(a 1) '(a 2))))
    (is (= nil (pattern-match '(a) '(a b c) ))) ;TODO not working I think  
    (is (= nil (pattern-match '(a b c) '(a)))))
  (testing "binding"
    (is (= {:var 2} (pattern-match '(a (? var)) '(a 2))))
    (is (= nil (pattern-match '(a (? var)) '(a))))
    (is (= {:var 1} (pattern-match '((? var) x (? var)) '(1 x 1))))
    (is (= nil (pattern-match '((? var) x (? var)) '(1 x 2))))
    (is (= '{:a x :b y} (pattern-match '((? a) (? b)) '(x y))))
    (is (= '{:a x} (pattern-match '((? a) (? a)) '(x x))))
    (is (= nil (pattern-match '((? a) (? a)) '(x y) )))))

(deftest uncollide-test
  (is (= '(1 2 3) (uncollide '(1 2 3))))
  (is (= '(1 2 4 3) (uncollide '(1 2 2 3) :new-key-fn #(* % 2))))
  (is (= '("a1" "a2-1") (uncollide '("a1" "a2") :existing '("a2") :new-key-fn #(str % "-1"))))
  (is (= '("a-1-1" "b") (uncollide '("a" "b") :existing '("a" "a-1") :new-key-fn #(str % "-1"))))
  )

(deftest intercalate-test
  (is (= '(a 1 b 2 c 3) (intercalate '(a b c) '(1 2 3))))
  (is (= '(a 1 b c) (intercalate '(a b c) '(1))))
  (is (= '(a 1 2 3) (intercalate '(a) '(1 2 3))))
  (is (= '(1 2 3) (intercalate nil '(1 2 3))))
  (is (= '(a b c) (intercalate '(a b c) nil))))

(deftest ignore-errors-test
  (testing "normal"
    (is (= 7 (ignore-errors (+ 3 4)))))
  (testing "error"
    (is (= nil (ignore-errors (/ 0 0) (+ 3 4))))))

(deftest error-handling-fn-test
  (let [ed (error-handling-fn /)]
    (is (= '(true 2/3) (ed 2 3)))
    (is (= '(false "Caught exception: java.lang.ArithmeticException: Divide by zero") (ed 2 0)))))

(deftest vectorize-test
  (let [+* (vectorize +)]
    (is (= 6 (+* 1 2 3)))
    (is (= [8 9 10]
           (+* 1 [4 5 6] 3)))
    (is (= [11 22 33]
           (+* [10 20 30] [1 2 3])))))

(deftest for*-test
  (is (= '([1 a] [2 b] [3 c])
         (for* [a '(1 2 3)
                b '(a b c)]
               [a b]))))

(deftest forcat-test
  (is (= '(1 2 3 7 8 9)
         (forcat [a [[1 2 3] [7 8] [9]]]
            a))))
        
(deftest str-replace-multiple-test
  (is (= "I like money and barfing on woo."
         (str-replace-multiple
          {"food" "money"
           "goof" "barf"}
          "I like food and goofing on woo."))))

(deftest re-substitute-test
  ;; Italicize all words that contain "oo"
  (is (= '("I like " [:i "food"] " and " [:i "goofing"] " on " [:i "woo"] ".")
         (re-substitute #"\w*oo\w*" "I like food and goofing on woo." (fn [ss] [:i ss])))))

(deftest dehumanize-test
  (let [m {"This" 1 "Uses strings" 2 "As keys" {"which is" "weird"}}]
    (is (= {:this 1, :uses_strings 2, :as_keys {:which_is "weird"}}
           (dehumanize m)))))

(deftest index-by-test
  (is (= '{a [a 1], b [b 2], c [c 3]}
         (index-by first '[[a 1] [b 2] [c 3]])))
  (is (= '{a [a 3], b [b 2]}
         (index-by first '[[a 1] [b 2] [a 3]]))))

(deftest index-by-safely-test
  (is (= '{a [a 1], b [b 2], c [c 3]}
         (index-by-safely first '[[a 1] [b 2] [c 3]])))
  (is (thrown? Exception
       (index-by-safely first '[[a 1] [b 2] [a 3]]))))

(deftest group-by-multiple-test
  (is (= {2 #{4 6 12 2 14 16 10 18 8}
          3 #{15 6 3 12 9 18}
          5 #{15 5 10}
          7 #{7 14}
          11 #{11}
          13 #{13}
          17 #{17}
          19 #{19}}
         (map-values set
                     (group-by-multiple math/prime-factors (range 2 20))))))

(deftest coerce-numeric-test
  (is (nil? (coerce-numeric nil)))
  (is (= 23 (coerce-numeric 23)))
  (is (= 23 (coerce-numeric "23")))
  (is (= "foo" (coerce-numeric "foo")))
  (is (= "" (coerce-numeric "")))
  (is (= + (coerce-numeric +)))
  (is (= 1.7 (coerce-numeric "1.7")))
  (is (= -1.7 (coerce-numeric "-1.7")))
  (is (= 1700.0 (coerce-numeric "1.7E3")))
  (is (= 0.017 (coerce-numeric "1.7E-2")))
  )

(deftest coerce-numeric-hard-test
  (is (nil? (coerce-numeric-hard nil)))
  (is (= 23 (coerce-numeric-hard 23)))
  (is (= 23 (coerce-numeric-hard "23")))
  (is (= nil (coerce-numeric-hard "foo")))
  (is (= nil (coerce-numeric-hard "")))
  (is (= nil (coerce-numeric-hard +)))
  (is (= 1.7 (coerce-numeric-hard "1.7")))
  (is (= -1.7 (coerce-numeric-hard "-1.7")))
  (is (= 1700.0 (coerce-numeric-hard "1.7E3")))
  )

;;; from Blood Meridian, Cormac McCarthy
(def text1 "They rode all day upon a pale gastine sparsely grown with saltbush and panicgrass. In the evening they entrained upon a hollow ground that rang so roundly under the horses' hooves that they stepped and sidled and rolled their eyes like circus animals and that night as they lay in that ground each heard, all heard, the dull boom of rock falling somewhere far below them in the awful darkness inside the world.")

(deftest collecting-test
  (is (= ["pale" "sparsely" "panicgrass"]
         (collecting
          (fn [collect]
            (doseq [word (nlp/tokens text1)]
              (when (re-find #"pa" word)
                (collect word))))))))

(deftest collecting-merge-test
  (let [silly
        (collecting-merge
         (fn [collect]
           (doseq [word (nlp/tokens text1)]
             (collect {(first word) [word]}))))]
    (is (= (get silly \h) '("hollow" "horses'" "hooves" "heard" "heard")))))

(deftest walk-map-entries-test
  (is (= {:a [1 1], :b [2 2], :c [{:d [3 3], :e [4 4]} {:d [3 3], :e [4 4]}]}
         (walk-map-entries (fn [[k v]] [k [v v]])
                           {:a 1 :b 2 :c {:d 3 :e 4 }}))))

(deftest walk-keys-test
  (is (= {:a 2, :b 2, :c {:d 6, :e 4}}
         (walk-keys (fn [[k v]]  [k (* 2 v)]) #{:a :d}
                    {:a 1 :b 2 :c {:d 3 :e 4}})))
  (is (= {:a 1, :b 2, :c {:d 6, :e 4}}
         (walk-keys (fn [[k v]]  [k (* 2 v)]) :d
                    {:a 1 :b 2 :c {:d 3 :e 4}}))))

(deftest walk-collect-test
  (is (= [1 2 3]
         (walk-collect (or-nil number?) {:a 1 :b [2 3]}))))

(deftest walk-find-test
  (is (= 2
         (walk-find (saferly even?)
                    {:a 1 :b [2 3]}))))

(deftest merge-recursive-test
  (is (= {:a 2}
         (merge-recursive {:a 1} {:a 2})))
  (is (= {:a {:x 1, :y 11, :z 22}}
         (merge-recursive {:a {:x 1 :y 2}} {:a {:y 11 :z 22}} )))
  (is (= {:a {:x 1 :y 2}}
         (merge-recursive {:a {:x 1 :y 2}} {:a nil} )))
  (is (= 23
         (merge-recursive {:a 1} 23)))
  (is (= {:a #{1 2 10 20}}
         (merge-recursive {:a #{1 2}} {:a #{10 20}})))
  (is (= '{:a [1 2 3 4]}
         (merge-recursive {:a [1 2]} '{:a [3 4]})))
  (is (= '{:a (1 2 3 4)}
         (merge-recursive {:a [1 2]} '{:a (3 4)})))
  )

(deftest union-by-test
  (let [words1 #{"this" "is" "kind" "of" "silly"}
        words2 #{"dont" "you" "think"}
        union (union-by count words1 words2)
        union-counts (set (map count union))]
    ;; the idea is union-by picks one word of each size; not well-defined which one it will be
    (is (= #{2 3 5 4} union-counts))))
    
(deftest safely-test
  (let [double-safe (safely double)]
    (is (= 23.0 (double-safe 23)))
    (is (nil? (double-safe nil)))))

(deftest saferly-test
  (let [even?-safe (saferly even?)]
    (is (= true (even?-safe 10)))
    (is (nil? (even?-safe "estragon")))
    (is (nil? (even?-safe nil)))))

(deftest partition-if-test
  (is (= '((0 1) (2 3) (4 5) (6 7) (8 9))
         (partition-if even? (range 10))))
  (is (= '()
         (partition-if even? '())))
  (is (= '((0))
         (partition-if even? '(0))))
  (is (= '((0 1) (2))
         (partition-if even? (range 3)))))

(deftest partition-diff-test
  (let [x '(1 9 2 0 3 7 2 2 7 2 7 7 5 7 8 6 5 4 2 1)]
    (is (= '((1 9) (2) (0 3 7) (2) (2 7) (2 7) (7) (5 7 8) (6) (5) (4) (2) (1))
           (partition-diff < x)))
    (is (= '((1) (9 2 0) (3) (7 2) (2) (7 2) (7) (7 5) (7) (8 6 5 4 2 1))
           (partition-diff > x)))))

(deftest substitute-test
  (let [vmap {:a :arnold :b :betty :c :claudio}
        struct [:a "likes" #{:b :c}]]
    (= [:arnold "likes" #{:betty :claudio}]
       (substitute struct vmap))))

(deftest substitute-gen-test
  (let [vmap {:a :arnold :b :betty :c :claudio}
        struct [:a "likes" #{:b :c} "likes" :d "hates" :e]
        result (substitute-gen struct vmap gensym)]
    (testing "gen called once per value"
      (is (= (nth result 1) (nth result 3))))
    (testing "gen d values are unique"
      (is (not (= (nth result 3) (nth result 5)))))))

(deftest pam-test
  (is (= (map #(* % 2) (range 10))
         (pam (range 10) #(* % 2)))))

(deftest clean-seq-test
  (is (= '(3 4 [a] "hey")
         (clean-seq '(3 nil 4 "" [] [a] "hey")))))

(deftest clean-map-test
  (is (= {:a 1 :c 3} (clean-map {:a 1 :b nil :c 3})))
  (is (= {} (clean-map {:a nil}))))

(deftest clean-maps-test
  (is (= {:a 1 :c 3} (clean-maps {:a 1 :b nil :c 3})))
  (is (= {} (clean-maps {:a nil})))
  (is (= {:a 1 :b {:x 1} :c 3} (clean-maps {:a 1 :b {:x 1 :y nil} :c 3}))))

(deftest all-keys-test
  (is (= #{:a :b :c :random}
         (all-keys [{:a 1 :b 2} {:a 3 :c 4} {} {:random :bits}]))))

(deftest stratify-test
  (let [results
        (stratify '{1 {:name a :predecessors []}
                    2 {:name b :predecessors [1]}
                    3 {:name c :predecessors [1]}
                    4 {:name d :predecessors [1 3]}
                    }
                  :predecessors :depth)]
    (is (= {1 0 2 1 3 1 4 2} (map-values :depth results))))
  (testing "consistency check"
    (is (thrown? AssertionError
                 (stratify  '{1 {:name a :predecessors []}
                              2 {:name b :predecessors [foo]}
                              3 {:name c :predecessors [1]}}
                            :predecessors :depth)))))

(deftest self-label-test
  (is (= '{1 {:name a :id 1}
           2 {:name b :id 2}}
         (self-label :id '{1 {:name a} 2 {:name b}}))))


(deftest add-inverse-test
  (let [db {:a {:children [:b :c]}
            :b {:children [:d]}}]
    (is (= {:a {:children [:b :c]}
            :b {:children [:d] :parent :a}
            :c {:parent :a} 
            :d {:parent :b}}
           (add-inverse db :children :parent)))))

(deftest add-inverse-multiple-test
  (let [db {:a {:children [:b :c]}
            :b {:children [:c]}}]
    (is (= {:a {:children [:b :c]}
            :b {:children [:c] :parents #{:a}}
            :c {:parents #{:a :b}}}
         (add-inverse-multiple db :children :parents)))))

(deftest fix-test
  (let [base-fib (fn [fib i]
                   (if (< i 2) 1 (+ (fib (- i 1)) (fib (- i 2)))))
        memoized-fib (fix (memoize base-fib))]
    ;; If this wasn't memoized, it would take forever
    (is (= 20365011074 (memoized-fib 50)))))

(deftest duplicates-test
  (is (empty? (duplicates (range 10))))
  (is (= '(1 2) (duplicates '(1 2 3 2 10 1 0 1)))))

  
