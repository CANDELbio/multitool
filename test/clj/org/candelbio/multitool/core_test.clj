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
  (testing "fix bad parse"
    (is (= "{'foo': foo}"
           (expand-template "{'{a}': {a}}" {:a "foo"} :key-fn keyword))))
  (testing "hyphens in var names"
      (is (= "{'foo': foo}"
             (expand-template "{'{a-ha}': {a-ha}}" {:a-ha "foo"} :key-fn keyword))))
  (testing "underscores in var names"
    (is (= "{'foo': foo}"
           (expand-template "{'{a_ha}': {a_ha}}" {:a_ha "foo"} :key-fn keyword))))
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

  
;;; get*

;;; TODO deidentify before checking in! And maybe move to resource
(def full-contact
  {:properties
   {:hubspot_owner_id
    {:value "35775960",
     :versions
     [{:value "35775960",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "264577",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :salesforceownername
    {:value "Michael Woodhouse",
     :versions
     [{:value "Michael Woodhouse",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "Kellan Gregory",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_analytics_last_url
    {:value
     "https://www.linkedin.com/ad/accounts/503208188/leadgen/view/2794956?hsa_acc=503208188&hsa_cam=613514706&hsa_grp=176653384&hsa_ad=129215964&hsa_src=&utm_campaign=Retargeting%2B-%2BFeed%2Bad&hsa_la=true&hsa_ol=false&hsa_net=linkedin&hsa_ver=3&utm_source=linkedin&utm_medium=paid",
     :versions
     [{:value
       "https://www.linkedin.com/ad/accounts/503208188/leadgen/view/2794956?hsa_acc=503208188&hsa_cam=613514706&hsa_grp=176653384&hsa_ad=129215964&hsa_src=&utm_campaign=Retargeting%2B-%2BFeed%2Bad&hsa_la=true&hsa_ol=false&hsa_net=linkedin&hsa_ver=3&utm_source=linkedin&utm_medium=paid",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "https://www.collaborativedrug.com/cdd-blog/lims-vs-eln",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value
       "https://info.collaborativedrug.com/tofu-ty-registration-scientific-webinar-2020-q1?submissionGuid=70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :num_conversion_events
    {:value "3",
     :versions
     [{:value "3",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "2",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value "1",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_is_unworked
    {:value "true",
     :versions
     [{:value "true",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}
      {:value "true",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_email_first_click_date
    {:value "1699449256151",
     :versions
     [{:value "1699449256151",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449256151,
       :selected false}]},
    :linkedin_profile_link
    {:value "https://www.linkedin.com/in/ketaki-ranade-594265163",
     :versions
     [{:value "https://www.linkedin.com/in/ketaki-ranade-594265163",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "linkedin_profile_link",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}]},
    :hs_analytics_last_referrer
    {:value "https://www.linkedin.com/",
     :versions
     [{:value "https://www.linkedin.com/",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value
       "https://info.collaborativedrug.com/tofu-ty-registration-scientific-webinar?submissionGuid=11a48402-d6d3-49ac-bd43-0b6be14dc82f",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "https://info.collaborativedrug.com/2023-q2-scientific-webinar",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_analytics_average_page_views
    {:value "1",
     :versions
     [{:value "1",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "2",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "3",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :first_conversion_event_name
    {:value
     "CDD Webinar: Artificial Intelligence Drug Discovery: Where are We Now?: 2023 Q2 Scientific Webinar",
     :versions
     [{:value
       "CDD Webinar: Artificial Intelligence Drug Discovery: Where are We Now?: 2023 Q2 Scientific Webinar",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_social_twitter_clicks
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :email
    {:value "ketaki.ranade@protonmail.com",
     :versions
     [{:value "ketaki.ranade@protonmail.com",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_marketable_status
    {:value "true",
     :versions
     [{:value "true",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_legal_basis
    {:value "Legitimate interest – prospect/lead;Not applicable",
     :versions
     [{:value "Legitimate interest – prospect/lead;Not applicable",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:583071390870;actionExecutionIndex:3",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684439951316,
       :selected false}
      {:value "Legitimate interest – prospect/lead",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label "Legal basis for processing contact's data",
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :conference
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :first_conversion_date
    {:value "1684436319286",
     :versions
     [{:value "1684436319286",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_predictivecontactscore_v2
    {:value "0.59",
     :versions
     [{:value "0.59",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1692360811015,
       :selected false}
      {:value "0.75",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1690853164559,
       :selected false}
      {:value "1.04",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1688972610891,
       :selected false}
      {:value "1.21",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1688367874217,
       :selected false}
      {:value "1.22",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1688043535024,
       :selected false}
      {:value "0.89",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1688043320306,
       :selected false}
      {:value "0.97",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1687935874108,
       :selected false}
      {:value "1.02",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1687503811546,
       :selected false}
      {:value "0.99",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1687158186123,
       :selected false}
      {:value "1.01",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1686294147185,
       :selected false}
      {:value "1.13",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1685948527216,
       :selected false}
      {:value "1.25",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1685603067336,
       :selected false}
      {:value "1.49",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1685379585500,
       :selected false}
      {:value "1.41",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1685343723310,
       :selected false}
      {:value "1.62",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1685000842539,
       :selected false}
      {:value "1.5",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1684998360083,
       :selected false}
      {:value "1.48",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1684794377034,
       :selected false}
      {:value "1.5",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1684738937881,
       :selected false}
      {:value "1.24",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1684436345342,
       :selected false}]},
    :hs_object_source_id
    {:value "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
     :versions
     [{:value "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-type "MIGRATION",
       :source-id "BackfillObjectSourceProperties",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1705021912321,
       :selected false}]},
    :currentlyinworkflow
    {:value "false",
     :versions
     [{:value "false",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889529063848",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706077010407,
       :selected false}
      {:value "true",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889535828835",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076402909,
       :selected false}
      {:value "false",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:793984534181",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449622426,
       :selected false}
      {:value "true",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:793984534181",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449319641,
       :selected false}
      {:value "false",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "65052086",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688972559883,
       :selected false}
      {:value "true",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "366208776",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436322162,
       :selected false}]},
    :hs_calculated_form_submissions
    {:value
     "0da753d1-0e0d-4171-92c7-d48f7266bf78:112316993065:1684436319286;570b02b7-f9e5-415f-a66f-eab5dcd27062:141110515976:1699449297278;63287d8c-a045-4041-8e3b-9b92a20fd117::1706076320120",
     :versions
     [{:value
       "0da753d1-0e0d-4171-92c7-d48f7266bf78:112316993065:1684436319286;570b02b7-f9e5-415f-a66f-eab5dcd27062:141110515976:1699449297278;63287d8c-a045-4041-8e3b-9b92a20fd117::1706076320120",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value
       "0da753d1-0e0d-4171-92c7-d48f7266bf78:112316993065:1684436319286;570b02b7-f9e5-415f-a66f-eab5dcd27062:141110515976:1699449297278",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value "0da753d1-0e0d-4171-92c7-d48f7266bf78:112316993065:1684436319286",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :ip_country
    {:value "canada",
     :versions
     [{:value "canada",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "united states",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "canada",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_v2_date_entered_marketingqualifiedlead
    {:value "1706076407193",
     :versions
     [{:value "1706076407193",
       :source-type "CALCULATED",
       :source-id "",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076419878,
       :selected false}]},
    :linkedin_consent_checkbox_i_agree_to_receive_email_from_cdd
    {:value "true",
     :versions
     [{:value "true",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "LinkedIn Consent: i_agree_to_receive_email_from_cdd.",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}]},
    :hs_email_last_send_date
    {:value "1710947066265",
     :versions
     [{:value "1710947066265",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710947066265,
       :selected false}
      {:value "1710428672690",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428672690,
       :selected false}
      {:value "1707397659680",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397659680,
       :selected false}
      {:value "1704978095149",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704978095149,
       :selected false}
      {:value "1704718848251",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718848251,
       :selected false}
      {:value "1702559072659",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702559072659,
       :selected false}
      {:value "1699448543398",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699448543398,
       :selected false}
      {:value "1695127649589",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1695127649589,
       :selected false}
      {:value "1694606652566",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1694606652566,
       :selected false}
      {:value "1692619355692",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692619355692,
       :selected false}
      {:value "1692360188186",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692360188186,
       :selected false}
      {:value "1688972579005",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688972579005,
       :selected false}
      {:value "1688367838943",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688367838943,
       :selected false}
      {:value "1688042849063",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042849063,
       :selected false}
      {:value "1687935816748",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687935816748,
       :selected false}
      {:value "1687503781799",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687503781799,
       :selected false}
      {:value "1687158176149",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687158176149,
       :selected false}
      {:value "1686639759805",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686639759805,
       :selected false}
      {:value "1686294130411",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686294130411,
       :selected false}
      {:value "1685948523015",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685948523015,
       :selected false}
      {:value "1685603012294",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685603012294,
       :selected false}
      {:value "1685343697802",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685343697802,
       :selected false}
      {:value "1684998179194",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684998179194,
       :selected false}
      {:value "1684738907235",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684738907235,
       :selected false}]},
    :hs_analytics_first_timestamp
    {:value "1684436298542",
     :versions
     [{:value "1684436298542",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :raastr
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_latest_source
    {:value "PAID_SOCIAL",
     :versions
     [{:value "PAID_SOCIAL",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "DIRECT_TRAFFIC",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "EMAIL_MARKETING",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "SOCIAL_MEDIA",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :lastname
    {:value "Ranade",
     :versions
     [{:value "Ranade",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label "LAST NAME",
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_analytics_last_visit_timestamp
    {:value "1706076320120",
     :versions
     [{:value "1706076320120",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "1699636636635",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "1699449258843",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "1684436298542",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :zoom_webinar_registration_count
    {:value "2",
     :versions
     [{:value "2",
       :source-type "INTEGRATION",
       :source-id "Zoom Integration",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449322075,
       :selected false}
      {:value "1",
       :source-type "INTEGRATION",
       :source-id "Zoom Integration",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436325089,
       :selected false}]},
    :hs_all_contact_vids
    {:value "378851327;390978977",
     :versions
     [{:value "378851327;390978977",
       :source-type "MERGE_OBJECTS",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259892,
       :selected false}
      {:value "378851327",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_social_linkedin_clicks
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_last_sales_activity_type
    {:value "FORM_SUBMITTED",
     :versions
     [{:value "FORM_SUBMITTED",
       :source-type "SALES",
       :source-id "last-sales-activity",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076357995,
       :selected false}
      {:value "HUBSPOT_REVISIT",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300933,
       :selected false}
      {:value "FORM_SUBMITTED",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300802,
       :selected false}
      {:value "HUBSPOT_REVISIT",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436325882,
       :selected false}]},
    :city
    {:value "Burnaby",
     :versions
     [{:value "Burnaby",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "city",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}]},
    :hs_latest_source_timestamp
    {:value "1706076320120",
     :versions
     [{:value "1706076320120",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "1699636636635",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "1699449258843",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "1684436298542",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_sequences_actively_enrolled_count
    {:value "0",
     :versions
     [{:value "0",
       :source-type "CALCULATED",
       :source-id "RollupProperties",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436321608,
       :selected false}]},
    :hs_analytics_first_referrer
    {:value "android-app://com.linkedin.android/",
     :versions
     [{:value "android-app://com.linkedin.android/",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :clr_region
    {:value "North America",
     :versions
     [{:value "North America",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889535828835;actionExecutionIndex:0",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076402250,
       :selected false}]},
    :hs_marketable_reason_id
    {:value "63287d8c-a045-4041-8e3b-9b92a20fd117",
     :versions
     [{:value "63287d8c-a045-4041-8e3b-9b92a20fd117",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "570b02b7-f9e5-415f-a66f-eab5dcd27062",
       :source-type "FORM",
       :source-id "11a48402-d6d3-49ac-bd43-0b6be14dc82f",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value "0da753d1-0e0d-4171-92c7-d48f7266bf78",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_marketable_reason_type
    {:value "AD",
     :versions
     [{:value "AD",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "FORM_SUBMISSION",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_analytics_source
    {:value "SOCIAL_MEDIA",
     :versions
     [{:value "SOCIAL_MEDIA",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_analytics_source_data_1
    {:value "LinkedIn",
     :versions
     [{:value "LinkedIn",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_email_first_open_date
    {:value "1684794136658",
     :versions
     [{:value "1684794136658",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684794136658,
       :selected false}]},
    :hubspot_owner_assigneddate
    {:value "1706106695317",
     :versions
     [{:value "1706106695317",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "1684436355518",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_lifecyclestage_marketingqualifiedlead_date
    {:value "1706076407193",
     :versions
     [{:value "1706076407193",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889529063848;actionExecutionIndex:1",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076407193,
       :selected false}]},
    :hs_analytics_last_touch_converting_campaign
    {:value "48974ba9-b6f3-47ef-90ef-a95c84ec9718",
     :versions
     [{:value "48974ba9-b6f3-47ef-90ef-a95c84ec9718",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_email_first_send_date
    {:value "1684738907235",
     :versions
     [{:value "1684738907235",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684738907235,
       :selected false}]},
    :hs_email_click
    {:value "1",
     :versions
     [{:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449303186,
       :selected false}]},
    :state
    {:value "quebec",
     :versions
     [{:value "quebec",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_social_google_plus_clicks
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_v2_latest_time_in_lead
    {:value "21640087407",
     :versions
     [{:value "21640087407",
       :source-type "CALCULATED",
       :source-id "",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076419850,
       :selected false}]},
    :hs_analytics_num_page_views
    {:value "7",
     :versions
     [{:value "7",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "6",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "3",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_all_assigned_business_unit_ids
    {:value "0",
     :versions
     [{:value "0",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_predictivescoringtier
    {:value "tier_2",
     :versions
     [{:value "tier_2",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1699530127309,
       :selected false}
      {:value "tier_1",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label "HubSpot Predictive Contact Scoring Model",
       :updated-by-user-id nil,
       :timestamp 1684436345342,
       :selected false}]},
    :lastmodifieddate
    {:value "1715186227252",
     :versions
     [{:value "1715186227252",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1715186227252,
       :selected false}
      {:value "1710947070187",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710947070187,
       :selected false}
      {:value "1710947065175",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710947065175,
       :selected false}
      {:value "1710428678399",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428678399,
       :selected false}
      {:value "1710428675463",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428675463,
       :selected false}
      {:value "1707422110428",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707422110428,
       :selected false}
      {:value "1707421989116",
       :source-type "CALCULATED",
       :source-id "ContactTimeZoneSyncProcessor",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421989116,
       :selected false}
      {:value "1707421987786",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421987786,
       :selected false}
      {:value "1707421984913",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421984913,
       :selected false}
      {:value "1707421983489",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421983489,
       :selected false}
      {:value "1707397663662",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397663662,
       :selected false}
      {:value "1707397659151",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397659151,
       :selected false}
      {:value "1707397658243",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397658243,
       :selected false}
      {:value "1706886712924",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706886712924,
       :selected false}
      {:value "1706886710987",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706886710987,
       :selected false}
      {:value "1706106696074",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106696074,
       :selected false}
      {:value "1706106695373",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695373,
       :selected false}
      {:value "1706077118304",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889529063848",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706077118304,
       :selected false}
      {:value "1706076891565",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076891565,
       :selected false}
      {:value "1706076580764",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889535828835",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076580764,
       :selected false}
      {:value "1706076425890",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "1706076419882",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076419882,
       :selected false}
      {:value "1706076417681",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076417681,
       :selected false}
      {:value "1706076407193",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889529063848;actionExecutionIndex:1",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076407193,
       :selected false}
      {:value "1706076402250",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889535828835;actionExecutionIndex:0",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076402250,
       :selected false}
      {:value "1706076366919",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076366919,
       :selected false}
      {:value "1706076358006",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076358006,
       :selected false}
      {:value "1706076357975",
       :source-type "ADS",
       :source-id "InternalProcess",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076357975,
       :selected false}
      {:value "1706076356545",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "1704978098522",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704978098522,
       :selected false}
      {:value "1704718851983",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718851983,
       :selected false}
      {:value "1704718846533",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718846533,
       :selected false}
      {:value "1702559078901",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702559078901,
       :selected false}
      {:value "1702559073593",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702559073593,
       :selected false}
      {:value "1702559073338",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702559073338,
       :selected false}
      {:value "1699637047251",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699637047251,
       :selected false}
      {:value "1699636710749",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "1699636639809",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636639809,
       :selected false}
      {:value "1699530127346",
       :source-type "AI_GROUP",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699530127346,
       :selected false}
      {:value "1699449703555",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449703555,
       :selected false}
      {:value "1699449681535",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449681535,
       :selected false}
      {:value "1699449677521",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:793984534181",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449677521,
       :selected false}
      {:value "1699449444569",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449444569,
       :selected false}
      {:value "1699449342190",
       :source-type "API",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "1699449334513",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:793984534181",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449334513,
       :selected false}]},
    :hs_count_is_unworked
    {:value "1",
     :versions
     [{:value "1",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}
      {:value "1",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355595,
       :selected false}]},
    :do_not_call
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :bh_product_interest
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_all_owner_ids
    {:value "35775960",
     :versions
     [{:value "35775960",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695373,
       :selected false}
      {:value "264577",
       :source-type "CALCULATED",
       :source-id "PermissionsUpdater",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436356859,
       :selected false}]},
    :hs_analytics_last_timestamp
    {:value "1706076320120",
     :versions
     [{:value "1706076320120",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "1699636636635",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "1699449321196",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "1684436323405",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_email_last_click_date
    {:value "1699449256151",
     :versions
     [{:value "1699449256151",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449256151,
       :selected false}]},
    :lifecyclestage
    {:value "marketingqualifiedlead",
     :versions
     [{:value "marketingqualifiedlead",
       :source-type "AUTOMATION_PLATFORM",
       :source-id "enrollmentId:889529063848;actionExecutionIndex:1",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076407193,
       :selected false}
      {:value "lead",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_pipeline
    {:value "contacts-lifecycle-pipeline",
     :versions
     [{:value "contacts-lifecycle-pipeline",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :recent_conversion_event_name
    {:value "LinkedIn Lead Generation Ad: Lead ad form: Lead generation ad - 11/18/2020 1:23 PM",
     :versions
     [{:value "LinkedIn Lead Generation Ad: Lead ad form: Lead generation ad - 11/18/2020 1:23 PM",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value
       "2023 Q4 Scientific Webinar: FAIR (meta)-Data & Assay Annotation: 2023 Q4 Scientific Webinar",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value
       "CDD Webinar: Artificial Intelligence Drug Discovery: Where are We Now?: 2023 Q2 Scientific Webinar",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :firstname
    {:value "Ketaki",
     :versions
     [{:value "Ketaki",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label "FIRST NAME",
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_analytics_revenue
    {:value "0.0",
     :versions
     [{:value "0.0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :ip_city
    {:value "burnaby",
     :versions
     [{:value "burnaby",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "new york",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "montreal",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (2560786e-e34f-3577-8042-0d4e6c16288f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379375096,
       :selected false}
      {:value "toronto",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (805f08eb-fc8d-30c0-8213-2d47d0c72994)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000616288,
       :selected false}
      {:value "montreal",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :salesforceownerid
    {:value "00539000006UMXBAA4",
     :versions
     [{:value "00539000006UMXBAA4",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "00570000000sNRIAA2",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_count_is_worked
    {:value "0",
     :versions
     [{:value "0",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}
      {:value "0",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355595,
       :selected false}]},
    :ip_state
    {:value "british columbia",
     :versions
     [{:value "british columbia",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "new york",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "quebec",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (2560786e-e34f-3577-8042-0d4e6c16288f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379375096,
       :selected false}
      {:value "ontario",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (805f08eb-fc8d-30c0-8213-2d47d0c72994)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000616288,
       :selected false}
      {:value "quebec",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_is_contact
    {:value "true",
     :versions
     [{:value "true",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hubspotscore
    {:value "4",
     :versions
     [{:value "4",
       :source-type "WAL_INCREMENTAL",
       :source-id "score-rule-set@v1559068567348",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699637018683,
       :selected false}
      {:value "2",
       :source-type "WAL_INCREMENTAL",
       :source-id "score-rule-set@v1559068567348",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449690000,
       :selected false}
      {:value "0",
       :source-type "WAL_INCREMENTAL",
       :source-id "csrsu-kafka@v1559068567348",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436857801,
       :selected false}]},
    :hs_analytics_num_visits
    {:value "4",
     :versions
     [{:value "4",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "3",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "2",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "1",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_timezone
    {:value "america_slash_vancouver",
     :versions
     [{:value "america_slash_vancouver",
       :source-type "CALCULATED",
       :source-id "ContactTimeZoneSyncProcessor",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421989116,
       :selected false}
      {:value "america_slash_new_york",
       :source-type "CALCULATED",
       :source-id "ContactTimeZoneSyncProcessor",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449301449,
       :selected false}
      {:value "america_slash_toronto",
       :source-type "CALCULATED",
       :source-id "ContactTimeZoneSyncProcessor",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436329073,
       :selected false}]},
    :hs_analytics_first_visit_timestamp
    {:value "1684436298542",
     :versions
     [{:value "1684436298542",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_latest_source_data_1
    {:value "LinkedIn",
     :versions
     [{:value "LinkedIn",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "www.collaborativedrug.com/cdd-blog/lims-vs-eln",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "2023 q4 scientific webinar",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}
      {:value "LinkedIn",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_time_to_move_from_lead_to_customer
    {:value "",
     :versions
     [{:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]},
    :hs_object_source_label
    {:value "FORM",
     :versions
     [{:value "FORM",
       :source-type "MIGRATION",
       :source-id "BackfillObjectSourceProperties",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1705021912321,
       :selected false}]},
    :hs_user_ids_of_all_owners
    {:value "7668441",
     :versions
     [{:value "7668441",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695373,
       :selected false}
      {:value "937070",
       :source-type "CALCULATED",
       :source-id "PermissionsUpdater",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436356859,
       :selected false}]},
    :time_to_ownership
    {:value "30288124",
     :versions
     [{:value "30288124",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695373,
       :selected false}
      {:value "21640051675",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076407193,
       :selected false}
      {:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]},
    :hs_last_sales_activity_timestamp
    {:value "1706076320120",
     :versions
     [{:value "1706076320120",
       :source-type "SALES",
       :source-id "last-sales-activity",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076357995,
       :selected false}
      {:value "1699636636635",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636639793,
       :selected false}
      {:value "1699449321196",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449324724,
       :selected false}
      {:value "1699449299059",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300933,
       :selected false}
      {:value "1699449297278",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300802,
       :selected false}
      {:value "1699449258843",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449261297,
       :selected false}
      {:value "1684436323405",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436325882,
       :selected false}]},
    :hs_marketable_until_renewal
    {:value "false",
     :versions
     [{:value "false",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_linkedin_ad_clicked
    {:value "true",
     :versions
     [{:value "true",
       :source-type "ADS",
       :source-id "InternalProcess",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076357950,
       :selected false}]},
    :recent_conversion_date
    {:value "1706076320120",
     :versions
     [{:value "1706076320120",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "1699449297278",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value "1684436319286",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :zoom_webinar_joinlink
    {:value
     "https://cddvault.zoom.us/w/84014957544?tk=-lFNWxr2NAOEe7PJdsrZEq7xde95kIsPhCCw4MW60Do.DQUAAAATj66D6BZMY1BfeVU0WlJnYUF2bGY2Q3pQS29RAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
     :versions
     [{:value
       "https://cddvault.zoom.us/w/84014957544?tk=-lFNWxr2NAOEe7PJdsrZEq7xde95kIsPhCCw4MW60Do.DQUAAAATj66D6BZMY1BfeVU0WlJnYUF2bGY2Q3pQS29RAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
       :source-type "INTEGRATION",
       :source-id "Zoom Integration",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449322075,
       :selected false}
      {:value
       "https://cddvault.zoom.us/w/81853563620?tk=Inc6jWRWoN-W8DauT0z3xxyHmRjtnczfXTJGnZVttKY.DQMAAAATDtpC5BY0NEtPZEoyUFRtYWpHM0ZDUHViaW13AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
       :source-type "INTEGRATION",
       :source-id "Zoom Integration",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436325089,
       :selected false}]},
    :hs_email_delivered
    {:value "24",
     :versions
     [{:value "24",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710947067552,
       :selected false}
      {:value "23",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428673462,
       :selected false}
      {:value "22",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397661113,
       :selected false}
      {:value "21",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704978096139,
       :selected false}
      {:value "20",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718849050,
       :selected false}
      {:value "19",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702559074097,
       :selected false}
      {:value "18",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699448544792,
       :selected false}
      {:value "17",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1695127650758,
       :selected false}
      {:value "16",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1694606654051,
       :selected false}
      {:value "15",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692619357544,
       :selected false}
      {:value "14",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692360189786,
       :selected false}
      {:value "13",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688972579469,
       :selected false}
      {:value "12",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688367839238,
       :selected false}
      {:value "11",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042850255,
       :selected false}
      {:value "10",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687935817114,
       :selected false}
      {:value "9",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687503782599,
       :selected false}
      {:value "8",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687158177046,
       :selected false}
      {:value "7",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686639761165,
       :selected false}
      {:value "6",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686294130912,
       :selected false}
      {:value "5",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685948523760,
       :selected false}
      {:value "4",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685603012925,
       :selected false}
      {:value "3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685343698501,
       :selected false}
      {:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684998180046,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684738908177,
       :selected false}]},
    :hs_lead_status
    {:value "Cold",
     :versions
     [{:value "Cold",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706886710959,
       :selected false}
      {:value "Warm",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "OPEN",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :salesforcelastsynctime
    {:value "1715186214141",
     :versions
     [{:value "1715186214141",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1715186227232,
       :selected false}
      {:value "1707422108506",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707422110376,
       :selected false}
      {:value "1706886708854",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706886712907,
       :selected false}
      {:value "1706106694206",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106696056,
       :selected false}
      {:value "1706076890148",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076891549,
       :selected false}
      {:value "1706076415498",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076417663,
       :selected false}
      {:value "1706076365225",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076366896,
       :selected false}
      {:value "1699449679749",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449681516,
       :selected false}
      {:value "1699449311892",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449313377,
       :selected false}
      {:value "1699449301833",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449303457,
       :selected false}
      {:value "1698912111282",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1698912147844,
       :selected false}
      {:value "1698864914965",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1698864927706,
       :selected false}
      {:value "1685379546533",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379549054,
       :selected false}
      {:value "1685000836069",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000838776,
       :selected false}
      {:value "1684437041067",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684437043384,
       :selected false}
      {:value "1684436361982",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436364408,
       :selected false}
      {:value "1684436351505",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_last_sales_activity_date
    {:value "1706076320",
     :versions
     [{:value "1706076320",
       :source-type "SALES",
       :source-id "last-sales-activity",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076357995,
       :selected false}
      {:value "1699636636",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636639793,
       :selected false}
      {:value "1699449321",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449324724,
       :selected false}
      {:value "1699449299",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300933,
       :selected false}
      {:value "1699449297",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449300802,
       :selected false}
      {:value "1699449258",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449261297,
       :selected false}
      {:value "1684436323",
       :source-type "SALES",
       :source-id "LastSalesActivityDateUpdateHandler",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436325882,
       :selected false}]},
    :hs_object_source
    {:value "FORM",
     :versions
     [{:value "FORM",
       :source-type "MIGRATION",
       :source-id "BackfillObjectSourceProperties",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1705021912321,
       :selected false}]},
    :hs_social_num_broadcast_clicks
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_email_open
    {:value "6",
     :versions
     [{:value "6",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421982029,
       :selected false}
      {:value "5",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449443022,
       :selected false}
      {:value "4",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688043114048,
       :selected false}
      {:value "3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379543021,
       :selected false}
      {:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000830764,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684794345228,
       :selected false}]},
    :num_unique_conversion_events
    {:value "3",
     :versions
     [{:value "3",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076356545,
       :selected false}
      {:value "2",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449298911,
       :selected false}
      {:value "1",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_analytics_num_event_completions
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :l_p_executives
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :ip_state_code
    {:value "bc",
     :versions
     [{:value "bc",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "ny",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "qc",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (2560786e-e34f-3577-8042-0d4e6c16288f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379375096,
       :selected false}
      {:value "on",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (805f08eb-fc8d-30c0-8213-2d47d0c72994)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000616288,
       :selected false}
      {:value "qc",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :hs_social_facebook_clicks
    {:value "0",
     :versions
     [{:value "0",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :jobtitle
    {:value "Student",
     :versions
     [{:value "Student",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076366896,
       :selected false}
      {:value "Laboratory Clerk",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "jobtitle",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}
      {:value "Student",
       :source-type "FORM",
       :source-id "11a48402-d6d3-49ac-bd43-0b6be14dc82f",
       :source-label "Job Title",
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}]},
    :salesforcedeleted
    {:value "false",
     :versions
     [{:value "false",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :salesforceowneremail
    {:value "mwoodhouse@collaborativedrug.com",
     :versions
     [{:value "mwoodhouse@collaborativedrug.com",
       :source-type "SALESFORCE",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706106695317,
       :selected false}
      {:value "kgregory@collaborativedrug.com",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_email_domain
    {:value "protonmail.com",
     :versions
     [{:value "protonmail.com",
       :source-type "CALCULATED",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_email_last_email_name
    {:value "2024 Q1 Webinar Email #3",
     :versions
     [{:value "2024 Q1 Webinar Email #3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710946885667,
       :selected false}
      {:value "2024 Q1 Webinar Email #2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428484272,
       :selected false}
      {:value "2024 Q1 Webinar Email #1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397222976,
       :selected false}
      {:value
       "Celebrating 20 Years of Pioneering Drug Discovery and Collaboration: CDD User Group Meeting",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704978027175,
       :selected false}
      {:value "2024 Upcoming Webinar- Manage Sample Inventory in CDD Vault",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718819175,
       :selected false}
      {:value "2023 Q4 Webinar Follow Up",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702558818915,
       :selected false}
      {:value "2023 Q4 Webinar Email #1 (2023 Q4 Scientific Webinar)",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699448410725,
       :selected false}
      {:value "Manage Protocol/Assay Definitions in CDD Vault",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1695127512586,
       :selected false}
      {:value "2023 Q3 Webinar Email #3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1694606445944,
       :selected false}
      {:value
       "CDD recognized in the 2023 Gartner® Hype Cycle™ for Life Science Discovery Research report",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692619235717,
       :selected false}
      {:value "2023 Q3 Webinar Email #1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692360032060,
       :selected false}
      {:value "2021 Lead Nurture 12 - Quote request",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688972559263,
       :selected false}
      {:value "2021 Lead Nurture 11 - Virtual team",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688367829992,
       :selected false}
      {:value "2023 Q2 Webinar Follow Up",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042709450,
       :selected false}
      {:value "2021 Lead Nurture 10 - LIMS",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687935813349,
       :selected false}
      {:value "2021 Lead Nurture 09 - Free trial",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687503779075,
       :selected false}
      {:value "2021 Lead Nurture 08 - Support",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687158157715,
       :selected false}
      {:value "2021 Lead Nurture 07 - Ecosystem",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686639742578,
       :selected false}
      {:value "2021 Lead Nurture 06 - Registration system",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686294118031,
       :selected false}
      {:value "2021 Lead Nurture 05 - Personalized demo",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685948511099,
       :selected false}
      {:value "2021 Lead Nurture 04 - Customer testimonial",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685602951082,
       :selected false}
      {:value "2021 Lead Nurture 03 - Collaboration",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685343689132,
       :selected false}
      {:value "2021 Lead Nurture 02 - ELN pillar",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684998168288,
       :selected false}
      {:value "2021 Lead Nurture 01 - Managing Data",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684738899311,
       :selected false}]},
    :hs_email_sends_since_last_engagement
    {:value "2",
     :versions
     [{:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710946885667,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1710428484272,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "4",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707397222976,
       :selected false}
      {:value "3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704978027175,
       :selected false}
      {:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1704718819175,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1702558818915,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449256151,
       :selected false}
      {:value "7",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699448410725,
       :selected false}
      {:value "6",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1695127512586,
       :selected false}
      {:value "5",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1694606445944,
       :selected false}
      {:value "4",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692619235717,
       :selected false}
      {:value "3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1692360032060,
       :selected false}
      {:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688972559263,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688367829992,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042869526,
       :selected false}
      {:value "8",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042709450,
       :selected false}
      {:value "7",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687935813349,
       :selected false}
      {:value "6",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687503779075,
       :selected false}
      {:value "5",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1687158157715,
       :selected false}
      {:value "4",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686639742578,
       :selected false}
      {:value "3",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1686294118031,
       :selected false}
      {:value "2",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685948511099,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685602951082,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379375096,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685343689132,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000616288,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684998168288,
       :selected false}
      {:value "0",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684794136658,
       :selected false}
      {:value "1",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684738899311,
       :selected false}]},
    :hs_object_id
    {:value "378851327",
     :versions
     [{:value "378851327",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :ip_country_code
    {:value "ca",
     :versions
     [{:value "ca",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "us",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "ca",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :createdate
    {:value "1684436319786",
     :versions
     [{:value "1684436319786",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_v2_date_exited_lead
    {:value "1706076407193",
     :versions
     [{:value "1706076407193",
       :source-type "CALCULATED",
       :source-id "",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076419854,
       :selected false}]},
    :country
    {:value "Canada",
     :versions
     [{:value "Canada",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "country",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}]},
    :hs_time_between_contact_creation_and_deal_close
    {:value "",
     :versions
     [{:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]},
    :hs_lifecyclestage_lead_date
    {:value "1684436319786",
     :versions
     [{:value "1684436319786",
       :source-type "FORM",
       :source-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :days_to_close
    {:value "",
     :versions
     [{:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]},
    :hs_ip_timezone
    {:value "america_slash_vancouver",
     :versions
     [{:value "america_slash_vancouver",
       :source-type "HEISENBERG",
       :source-id "EMAIL_OPEN (fe68fda0-b5f7-3e86-9ee4-f6dcd23d92f4)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "america_slash_new_york",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (11a48402-d6d3-49ac-bd43-0b6be14dc82f)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449297278,
       :selected false}
      {:value "america_slash_toronto",
       :source-type "HEISENBERG",
       :source-id "FORM_SUBMISSION (70dc6ff5-c0ca-457d-a58d-a175b2007a8b)",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319286,
       :selected false}]},
    :company
    {:value "Concordia University",
     :versions
     [{:value "Concordia University",
       :source-type "FORM",
       :source-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
       :source-label "company",
       :updated-by-user-id nil,
       :timestamp 1706076320120,
       :selected false}]},
    :hs_latest_source_data_2
    {:value "retargeting - feed ad",
     :versions
     [{:value "retargeting - feed ad",
       :source-type "ANALYTICS",
       :source-id "WebAnalyticsPropertyCalculation",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076425890,
       :selected false}
      {:value "",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699636710749,
       :selected false}
      {:value "281105708",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449342190,
       :selected false}]},
    :salesforceleadid
    {:value "00Q3n00001g83PLEAY",
     :versions
     [{:value "00Q3n00001g83PLEAY",
       :source-type "SALESFORCE",
       :source-id "Salesforce Connector",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436355518,
       :selected false}]},
    :hs_v2_date_entered_lead
    {:value "1684436319786",
     :versions
     [{:value "1684436319786",
       :source-type "MIGRATION",
       :source-id "CrmScpBackfillBatchProcessor",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436319786,
       :selected false}]},
    :hs_analytics_first_url
    {:value
     "https://www.collaborativedrug.com/upcoming-webinar-artificial-intelligence-drug-discovery-where-are-we-now/",
     :versions
     [{:value
       "https://www.collaborativedrug.com/upcoming-webinar-artificial-intelligence-drug-discovery-where-are-we-now/",
       :source-type "ANALYTICS",
       :source-id "ContactAnalyticsDetailsUpdateWorker",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684436330455,
       :selected false}]},
    :hs_v2_cumulative_time_in_lead
    {:value "21640087407",
     :versions
     [{:value "21640087407",
       :source-type "CALCULATED",
       :source-id "",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1706076419849,
       :selected false}]},
    :hs_email_last_open_date
    {:value "1707421816900",
     :versions
     [{:value "1707421816900",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1707421816900,
       :selected false}
      {:value "1699449230356",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449230356,
       :selected false}
      {:value "1688042869526",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1688042869526,
       :selected false}
      {:value "1685379375096",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685379375096,
       :selected false}
      {:value "1685000616288",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1685000616288,
       :selected false}
      {:value "1684794136658",
       :source-type "EMAIL",
       :source-id nil,
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1684794136658,
       :selected false}]},
    :hs_time_between_contact_creation_and_deal_creation
    {:value "",
     :versions
     [{:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]},
    :hs_sequences_is_enrolled
    {:value "",
     :versions
     [{:value "",
       :source-type "CALCULATED",
       :source-id "CalculatedPropertyComputer",
       :source-label nil,
       :updated-by-user-id nil,
       :timestamp 1699449259862,
       :selected false}]}},
   :vid 378851327,
   :identity-profiles
   [{:vid 378851327,
     :saved-at-timestamp 1699449259862,
     :deleted-changed-timestamp 0,
     :identities
     [{:type "EMAIL",
       :value "ketaki.ranade@protonmail.com",
       :timestamp 1684436319286,
       :is-primary true}
      {:type "LEAD_GUID", :value "58d335a8-2f00-43b7-9ddc-21752af6fab7", :timestamp 1684436319850}]}
    {:vid 390978977,
     :saved-at-timestamp 1699449259862,
     :deleted-changed-timestamp 0,
     :identities []}],
   :form-submissions
   [{:conversion-id "ea925e93-09e3-4d29-a4c9-edacbfd9b4f4",
     :contact-associated-by ["EMAIL"],
     :page-url
     "https://www.linkedin.com/ad/accounts/503208188/leadgen/view/2794956?hsa_acc=503208188&hsa_cam=613514706&hsa_grp=176653384&hsa_ad=129215964&hsa_src=&utm_campaign=Retargeting%2B-%2BFeed%2Bad&hsa_la=true&hsa_ol=false&hsa_net=linkedin&hsa_ver=3&utm_source=linkedin&utm_medium=paid",
     :meta-data [],
     :title "Lead ad form: Lead generation ad - 11/18/2020 1:23 PM",
     :page-title "LinkedIn Lead Generation Ad",
     :portal-id 146552,
     :form-id "63287d8c-a045-4041-8e3b-9b92a20fd117",
     :timestamp 1706076320120,
     :form-type "FACEBOOK_LEAD_AD"}
    {:conversion-id "11a48402-d6d3-49ac-bd43-0b6be14dc82f",
     :contact-associated-by ["EMAIL"],
     :page-url
     "https://info.collaborativedrug.com/2023-q4-scientific-webinar?utm_campaign=2023%20Q4%20Scientific%20Webinar&utm_medium=email&_hsmi=281539225&_hsenc=p2ANqtz-91685bMZ-FUg7fwuK8K4L6zay9m4peYSJxmr2cvAPh7K_6ZI3ADLZO_CXenl2A9ZZmiKal-qbKvXIL_-yckTVWFUhZ4odDm5p4cQ2kqMnPjjoeDJQ&utm_content=281105708&utm_source=hs_email",
     :meta-data [],
     :title "2023 Q4 Scientific Webinar",
     :page-id "141110515976",
     :canonical-url "https://info.collaborativedrug.com/2023-q4-scientific-webinar",
     :page-title "2023 Q4 Scientific Webinar: FAIR (meta)-Data & Assay Annotation",
     :portal-id 146552,
     :content-type "landing-page",
     :form-id "570b02b7-f9e5-415f-a66f-eab5dcd27062",
     :timestamp 1699449297278,
     :form-type "HUBSPOT"}
    {:conversion-id "70dc6ff5-c0ca-457d-a58d-a175b2007a8b",
     :page-url "https://info.collaborativedrug.com/2023-q2-scientific-webinar",
     :meta-data [],
     :title "2023 Q2 Scientific Webinar",
     :page-id "112316993065",
     :canonical-url "https://info.collaborativedrug.com/2023-q2-scientific-webinar",
     :page-title "CDD Webinar: Artificial Intelligence Drug Discovery: Where are We Now?",
     :portal-id 146552,
     :content-type "landing-page",
     :form-id "0da753d1-0e0d-4171-92c7-d48f7266bf78",
     :timestamp 1684436319286,
     :form-type "HUBSPOT"}],
   :list-memberships
   [{:static-list-id 200,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 223,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 228,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 475,
     :internal-list-id 2147483643,
     :timestamp 1684436348933,
     :vid 378851327,
     :is-member true}
    {:static-list-id 477,
     :internal-list-id 2147483643,
     :timestamp 1684439951996,
     :vid 378851327,
     :is-member true}
    {:static-list-id 479,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 486,
     :internal-list-id 2147483643,
     :timestamp 1684439951886,
     :vid 378851327,
     :is-member true}
    {:static-list-id 488,
     :internal-list-id 2147483643,
     :timestamp 1684439960212,
     :vid 378851327,
     :is-member true}
    {:static-list-id 505,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 577,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 626,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 685,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 765,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 882,
     :internal-list-id 2147483643,
     :timestamp 1684436355109,
     :vid 378851327,
     :is-member true}
    {:static-list-id 899,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 900,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 993,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1008,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1021,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1022,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1231,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1240,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1295,
     :internal-list-id 2147483643,
     :timestamp 1684436355109,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1354,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1409,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1418,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1426,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1468,
     :internal-list-id 2147483643,
     :timestamp 1706076457956,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1470,
     :internal-list-id 2147483643,
     :timestamp 1706076457939,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1472,
     :internal-list-id 2147483643,
     :timestamp 1706076394444,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1494,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1532,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1538,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1546,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1581,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1594,
     :internal-list-id 2147483643,
     :timestamp 1684436348963,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1596,
     :internal-list-id 2147483643,
     :timestamp 1684436348994,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1628,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1634,
     :internal-list-id 2147483643,
     :timestamp 1706076394444,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1636,
     :internal-list-id 2147483643,
     :timestamp 1706076394444,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1637,
     :internal-list-id 2147483643,
     :timestamp 1706076458126,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1639,
     :internal-list-id 2147483643,
     :timestamp 1706077038124,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1682,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1839,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1861,
     :internal-list-id 2147483643,
     :timestamp 1684436362163,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1863,
     :internal-list-id 2147483643,
     :timestamp 1688972570912,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1869,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1876,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 1990,
     :internal-list-id 2147483643,
     :timestamp 1706076431013,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2054,
     :internal-list-id 2147483643,
     :timestamp 1713304112048,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2128,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2205,
     :internal-list-id 2147483643,
     :timestamp 1684436340138,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2399,
     :internal-list-id 2147483643,
     :timestamp 1684436340137,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2400,
     :internal-list-id 2147483643,
     :timestamp 1684436326413,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2402,
     :internal-list-id 2147483643,
     :timestamp 1684436631116,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2404,
     :internal-list-id 2147483643,
     :timestamp 1684436340137,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2505,
     :internal-list-id 2147483643,
     :timestamp 1699449353884,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2507,
     :internal-list-id 2147483643,
     :timestamp 1699449647383,
     :vid 378851327,
     :is-member true}
    {:static-list-id 2512,
     :internal-list-id 2147483643,
     :timestamp 1699449318437,
     :vid 378851327,
     :is-member true}],
   :portal-id 146552,
   :merge-audits [],
   :is-contact true,
   :merged-vids [],
   :canonical-vid 378851327})

(deftest get-in*-test
  (testing "basic"
    (let [x {:a 1 :b [2 3 4] :c [{:n 10} {:n 11}]}]
      ;; NOTE tests rely on (= [1] '(1)) 
      (is (= [1] (get-in* x [:a])))
      (is (= [[2 3 4]] (get-in* x [:b])))
      (is (= [nil]) (get-in* x [:not]))   ;Not sure what is right here
      (is (= [10 11] (get-in* x [:c :* :n])))))
  (testing "actual"
    #_(is (=
           (get-in* full-contact [:properties :* :value] )))))
