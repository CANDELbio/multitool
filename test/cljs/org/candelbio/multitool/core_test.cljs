;;; Ideally merge this with the clj tests as a cljc file, but for now doing it separately

(ns org.candelbio.multitool.core-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [org.candelbio.multitool.core :as sut :refer-macros [ignore-errors doseq* for* forcat]]
            [clojure.string :as str]
            [org.candelbio.multitool.nlp :as nlp]
            [org.candelbio.multitool.math :as math]
            )
  )

(deftest truncate-string-test
  (is (= "foo" (sut/truncate-string "foo" 3)))
  (is (= "fo…" (sut/truncate-string "foo" 2)))
  (is (= "fo" (sut/truncate-string "fo" 3))))


(deftest memoize-named-test
  (let [counter (atom 0)
        next #(swap! counter inc)
        mem-next (sut/memoize-named :hey next)]
    (is (not (= (next) (next))))
    (is (= (mem-next) (mem-next)))
    (let [a (mem-next)]
      (sut/memoize-reset! :hey)
      (is (not (= (mem-next) a))))))

(deftest underscore->camelcase-test
  (is (= (sut/underscore->camelcase "foo_bar") "fooBar"))
  (is (= (sut/underscore->camelcase "foo") "foo")))

(deftest map-invert-multiple-test
  (is (= {} (sut/map-invert-multiple {})))
  (is (= '{1 #{:a}} (sut/map-invert-multiple {:a 1})))
  (is (= '{1 #{:a} :x #{:b} :y #{:b}} (sut/map-invert-multiple {:a 1 :b '(:x :y)})))
  (is (= '{1 #{:a :b} 2 #{:b} } (sut/map-invert-multiple {:a 1 :b '(1 2)})))
  )

(deftest map-chunked-test
  (let [f (fn [x] (* x 2))]
    (= (map f (range 100))
       (sut/map-chunked #(map f %) 7 (range 100)))))
      
(deftest following-elt-test
  (is (= 3 (sut/following-elt 2 [1 2 3 4])))
  (is (nil? (sut/following-elt 20 [1 2 3 4])))
  (is (nil? (sut/following-elt 4 [1 2 3 4])))
  (is (nil? (sut/following-elt 1 [])))
  (testing "stack safety"
    (is (= 90001 (sut/following-elt 90000 (range 100000))))))

(deftest preceding-elt-test
  (is (= 3 (sut/preceding-elt 4 [1 2 3 4])))
  (is (nil? (sut/preceding-elt 20 [1 2 3 4])))
  (is (nil? (sut/preceding-elt 1 [1 2 3 4])))
  (is (nil? (sut/preceding-elt 1 [])))
  (testing "stack safety"
    (is (= 89999 (sut/preceding-elt 90000 (range 100000))))))

(deftest some-thing-test
  (is (= 2 (sut/some-thing even? '(1 2 3 4)))))

(deftest repeat-until-test
  (is (= 16 (sut/repeat-until #(> % 10) #(* % 2) 1))))

(deftest remove=-test
  (is (= '(0 1 3 4) (sut/remove= 2 (range 5))))
  (is (= '(0 2 3 4) (sut/remove= 2 (range 5) #(* % 2)))))

(deftest delete-subseq-test
  (let [seq '(a b c d e f)]
    (= '(a e f g) (sut/delete-subseq seq '(b c c)))
    (= '(c d e f g) (sut/delete-subseq seq '(a b)))
    (= '(a b c d e) (sut/delete-subseq seq '(f g)))
    (= seq (sut/delete-subseq seq '(a x)))
    (= '() (sut/delete-subseq seq seq))))

(deftest positions-test
  (is (= '(0 2 4 6 8) (sut/positions even? '(0 1 2 3 4 3 2 1 0))))
  (is (= '(3 5) (sut/positions= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest position-test
  (is (= 1 (sut/position even? '(1 2 3 4 3 2 1 0))))
  (is (= nil (sut/position string? '(1 2 3 4 3 2 1 0))))
  (is (= 3 (sut/position= 3 '(0 1 2 3 4 3 2 1 0)))))

(deftest sort-with-numeric-prefix-test
  (is (= '("foo" "0001 foo" "2 foo" "100 demons")
         (sut/sort-with-numeric-prefix ["100 demons" "0001 foo" "2 foo" "foo"]))))

(deftest subseqs-test
  (let [seq5 '(a b c d e)]
    (is (= '((a b c) (b c d) (c d e)) (sut/subseqs seq5 3)))
    (is (= '((a b c d) (b c d e)) (sut/subseqs seq5 4)))
    (is (= '() (sut/subseqs seq5 10)))))

(deftest set=-test
  (is (sut/set= '(a b c) '(c b a)))
  (is (not (sut/set= '(a b c) '(c b d))))
  (is (sut/set= '(a b c) '(c b a c))))

(deftest bag=-test
  (is (sut/bag= '(a b c) '(c b a)))
  (is (not (sut/bag= '(a b c) '(c b d))))
  (is (not (sut/bag= '(a b c) '(c b a c))))
  (is (sut/bag= '(a c b c) '(c b a c))))

;;; Reference to Math/ is fatal to test runner, it hangs
#_
(deftest powerset-test
  (is (= #{#{} #{3} #{2} #{1} #{1 3 2} #{1 3} #{1 2} #{3 2}}
         (sut/powerset #{1 2 3})))
  (testing "works on empty set"
    (is (= #{#{}} (sut/powerset #{}))))
  (testing "works on lists"
    (is (= '#{#{a} #{c b} #{a b} #{} #{b} #{c} #{a c} #{a c b}}
           (sut/powerset '(a b c)))))
  (testing "works on decent sized list"
    (is (= (Math/round (Math/pow 2 18))
           (count (sut/powerset (range 18)))))))

(deftest transitive-closure-test
  (let [tree [:organisms
              [:plants [:trees] [:cacti]]
              [:animals
               [:mammals [:elephants] [:cats]]
               [:birds [:terns] [:warblers] [:owls]]]]
        children #(subvec % 1)
        descendents (sut/transitive-closure children)]
    (is (= (set (map first (children tree)))
           #{:plants :animals}))
    (is (= (set (map first (descendents tree)))
           #{:terns :cats :birds :elephants :plants :warblers :owls
             :trees :animals :organisms :mammals :cacti}))))

(deftest divide-with-test
  (= [[0 2 4 6 8] [1 3 5 7 9]]
     (sut/divide-with even? (range 10))))

(deftest compare-tests
  (is (sut/>* 2 1))
  (is (sut/>* "foo" "bar"))
  (is (not (sut/>* 1 1)))
  (is (sut/<* 2 3))
  (is (sut/<* "foo" "quux"))
  (is (not (sut/<* 1 1))))

(defn tokens [s]
  (str/split s #"\W"))

(deftest maxby-minby-test
  (is (= "tediously"
         (sut/max-by count (tokens "this is a tediously long string"))))
  (is (= "a"
         (sut/min-by count (tokens "this is a tediously long string"))))
  (is (nil? (sut/max-by count [])))
  (is (nil? (sut/min-by count []))))

(deftest min*-max*-test
  (let [words '("you" "call" "this" "living")]
    (is (= "call" (sut/min* words)))
    (is (= "you" (sut/max* words)))))


(deftest doseq*-test
  (let [acc (atom '())]
    (doseq* [a '(1 2 3)
             b '(x y z)]
            (swap! acc conj (list a b)))
    (= '((1 x) (2 y) (3 z))
       (reverse @acc)))
  ;; TODO seqs of different length
  )

#_
(deftest re-seq-positions-test
  (is (= [[7 10] [13 16] [28 31]]
         (sut/re-seq-positions #"foo" "I like food, fooseball, and foolishness.")))
  (is (= [[6 9] [15 16] [29 34]]
         (sut/re-seq-positions #"\((-*)\)" "This (---) is (-) something (-----) else" 1)))
  (is (= []
         (sut/re-seq-positions #"\((-*)\)" "nada"))))

(deftest treeword-test
  (is (= "yo"
         ((sut/treeword "foo.bar")
          {:foo {:bar "yo"}})))
  (testing "is just keyword if no ."
    (is (= "yo"
           ((sut/treeword "foo")
            {:foo "yo"}))))
  (testing "keyword input"
   (is (= :foo (sut/treeword :foo))))
  (testing "dotted keyword input"
    (is (= "yo"
           ((sut/treeword :foo.bar)
            {:foo {:bar "yo"}})))))

;;; TODO would make sense to have a re-seq variant that could return groups

(deftest expand-template-test
  
  (testing "Double braces (default)"
    (let [template "The {{foo}} must have {{bar}}!"
          bindings1 {:foo "subgenius" :bar "slack"}
          bindings2 {:foo "dog"}]
      (is (= "The subgenius must have slack!"
             (sut/expand-template template bindings1)))
      (is (= "The dog must have !"
             (sut/expand-template template bindings2))))
    )
  (testing "Javascript templating, keywords"
    (let [template "The ${foo} must have ${bar}!"
          bindings1 {:foo "subgenius" :bar "slack"}]
      (is (= "The subgenius must have slack!"
             (sut/expand-template template bindings1 :param-regex sut/param-regex-javascript)))
      ))
  (testing "fix bad parse"
    (is (= "{'foo': foo}"
           (sut/expand-template "{'{{a}}': {{a}}}" {:a "foo"}))))
  (testing "hyphens in var names"
    (is (= "{'foo': foo}"
           (sut/expand-template "{'{{a-ha}}': {{a-ha}}}" {:a-ha "foo"}))))
  (testing "underscores in var names"
    (is (= "{'foo': foo}"
           (sut/expand-template "{'{{a_ha}}': {{a_ha}}}" {:a_ha "foo"}))))
  (testing "dots in var names"
    (is (= "This is useful is it not"
           (sut/expand-template "This is {{quite.wack}} is it not"
                            {:quite {:wack "useful"}}))))
  ;; TODO test :keyword=fn arg
  )

(deftest expand-template-recur-test
  (testing "simple"
    (is (= "This is quite serious about vampires"
           (sut/expand-template-recur
            "This is {{silly}}"
            {:silly "quite serious about {{subject}}"
             :subject "vampires"}))))
  ;; this might be an ugly and overcomplicated way to do something simple
  (testing "realistic"
    (let [template
          "MATCH(e:PressRelease {}) MATCH (c:Company {{ticker-clause}})-[r]-(e)
WHERE {{time-filter-clause}}
{{limit-clause}}"
          expand (fn [{:keys [ticker limit start-date end-date] :as params}]
                   (sut/expand-template-recur
                    template
                    (merge
                     {:ticker-clause (if ticker "{ticker: '{{ticker}}'}" "{}")
                      :time-filter-clause 
                      (str/join " AND " (sut/mapf identity (list (when end-date "e.date < '{{end-date}}'")
                                                             (when start-date  "e.date > '{{start-date}}'")
                                                              "TRUE")))
                      :limit-clause (if limit "LIMIT {{limit}}" "")
                      }
                     params)))]
      (is (= "MATCH(e:PressRelease {}) MATCH (c:Company {ticker: 'RYTM'})-[r]-(e)\nWHERE TRUE\n"
             (expand {:ticker "RYTM"})))
      (is (= "MATCH(e:PressRelease {}) MATCH (c:Company {})-[r]-(e)\nWHERE e.date > '2024-02-02' AND TRUE\n"
             (expand {:start-date "2024-02-02"})))
      )))

#_
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
  (is (= '(1 2 3) (sut/uncollide '(1 2 3))))
  (is (= '(1 2 4 3) (sut/uncollide '(1 2 2 3) :new-key-fn #(* % 2))))
  (is (= '("a1" "a2-1") (sut/uncollide '("a1" "a2") :existing '("a2") :new-key-fn #(str % "-1"))))
  (is (= '("a-1-1" "b") (sut/uncollide '("a" "b") :existing '("a" "a-1") :new-key-fn #(str % "-1"))))
  )

(deftest intercalate-test
  (is (= '(a 1 b 2 c 3) (sut/intercalate '(a b c) '(1 2 3))))
  (is (= '(a 1 b c) (sut/intercalate '(a b c) '(1))))
  (is (= '(a 1 2 3) (sut/intercalate '(a) '(1 2 3))))
  (is (= '(1 2 3) (sut/intercalate nil '(1 2 3))))
  (is (= '(a b c) (sut/intercalate '(a b c) nil))))

;;; Note: (/ 0 0) does not throw an error in js! So these have diverged from clj version

(deftest ignore-errors-test
  (testing "normal"
    (is (= 7 (ignore-errors (+ 3 4)))))
  (testing "error"
    (is (= nil (ignore-errors (.foo 0) (+ 3 4))))))

;;; TODO should test success
(deftest error-handling-fn-test
  (let [f #(.foo %)
        ed (sut/error-handling-fn f)
        [success? msg] (ed 2)]
    (is (= false success?))
    (is (re-find #"foo is not a function" msg))))

(deftest vectorize-test
  (let [+* (sut/vectorize +)]
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
         (sut/str-replace-multiple
          {"food" "money"
           "goof" "barf"}
          "I like food and goofing on woo."))))

#_
(deftest re-substitute-test
  ;; Italicize all words that contain "oo"
  (is (= '("I like " [:i "food"] " and " [:i "goofing"] " on " [:i "woo"] ".")
         (re-substitute #"\w*oo\w*" "I like food and goofing on woo." (fn [ss] [:i ss])))))

(deftest dehumanize-test
  (let [m {"This" 1 "Uses strings" 2 "As keys" {"which is" "weird"}}]
    (is (= {:this 1, :uses_strings 2, :as_keys {:which_is "weird"}}
           (sut/dehumanize m)))))

(deftest index-by-test
  (is (= '{a [a 1], b [b 2], c [c 3]}
         (sut/index-by first '[[a 1] [b 2] [c 3]])))
  (is (= '{a [a 3], b [b 2]}
         (sut/index-by first '[[a 1] [b 2] [a 3]]))))

(deftest index-by-safely-test
  (is (= '{a [a 1], b [b 2], c [c 3]}
         (sut/index-by-safely first '[[a 1] [b 2] [c 3]])))
  (is (thrown? :default                 ;???
       (sut/index-by-safely first '[[a 1] [b 2] [a 3]]))))

(deftest group-by-multiple-test
  (is (= {2 #{4 6 12 2 14 16 10 18 8}
          3 #{15 6 3 12 9 18}
          5 #{15 5 10}
          7 #{7 14}
          11 #{11}
          13 #{13}
          17 #{17}
          19 #{19}}
         (sut/map-values set
                         (sut/group-by-multiple math/prime-factors (range 2 20))))))

(deftest coerce-numeric-test
  (is (nil? (sut/coerce-numeric nil)))
  (is (= 23 (sut/coerce-numeric 23)))
  (is (= 23 (sut/coerce-numeric "23")))
  (is (= "foo" (sut/coerce-numeric "foo")))
  (is (= "" (sut/coerce-numeric "")))
  (is (= + (sut/coerce-numeric +)))
  (is (= 1.7 (sut/coerce-numeric "1.7")))
  (is (= -1.7 (sut/coerce-numeric "-1.7")))
  (is (= 1700.0 (sut/coerce-numeric "1.7E3")))
  (is (= 0.017 (sut/coerce-numeric "1.7E-2")))
  )

(deftest coerce-numeric-hard-test
  (is (nil? (sut/coerce-numeric-hard nil)))
  (is (= 23 (sut/coerce-numeric-hard 23)))
  (is (= 23 (sut/coerce-numeric-hard "23")))
  (is (= nil (sut/coerce-numeric-hard "foo")))
  (is (= nil (sut/coerce-numeric-hard "")))
  (is (= nil (sut/coerce-numeric-hard +)))
  (is (= 1.7 (sut/coerce-numeric-hard "1.7")))
  (is (= -1.7 (sut/coerce-numeric-hard "-1.7")))
  (is (= 1700.0 (sut/coerce-numeric-hard "1.7E3")))
  )

;;; from Blood Meridian, Cormac McCarthy
(def text1 "They rode all day upon a pale gastine sparsely grown with saltbush and panicgrass. In the evening they entrained upon a hollow ground that rang so roundly under the horses' hooves that they stepped and sidled and rolled their eyes like circus animals and that night as they lay in that ground each heard, all heard, the dull boom of rock falling somewhere far below them in the awful darkness inside the world.")

(deftest collecting-test
  (is (= ["pale" "sparsely" "panicgrass"]
         (sut/collecting
          (fn [collect]
            (doseq [word (nlp/tokens text1)]
              (when (re-find #"pa" word)
                (collect word))))))))

(deftest collecting-set-test
  (is (= #{"pale" "sparsely" "panicgrass"}
         (sut/collecting-set
          (fn [collect]
            (doseq [word (nlp/tokens text1)]
              (when (re-find #"pa" word)
                (collect word))))))))

(deftest walk-map-entries-test
  (is (= {:a [1 1], :b [2 2], :c [{:d [3 3], :e [4 4]} {:d [3 3], :e [4 4]}]}
         (sut/walk-map-entries (fn [[k v]] [k [v v]])
                           {:a 1 :b 2 :c {:d 3 :e 4 }}))))

(deftest walk-keys-test
  (is (= {:a 2, :b 2, :c {:d 6, :e 4}}
         (sut/walk-keys (fn [[k v]]  [k (* 2 v)]) #{:a :d}
                    {:a 1 :b 2 :c {:d 3 :e 4}})))
  (is (= {:a 1, :b 2, :c {:d 6, :e 4}}
         (sut/walk-keys (fn [[k v]]  [k (* 2 v)]) :d
                    {:a 1 :b 2 :c {:d 3 :e 4}}))))


(deftest walk-collect-test
  (is (= [1 2 3]
         (sut/walk-collect (sut/or-nil number?) {:a 1 :b [2 3]}))))

(deftest walk-collect-set-test
  (is (= #{1 2 3}
         (sut/walk-collect-set (sut/or-nil number?) {:a 1 :b [2 3]}))))

(deftest walk-find-test
  (is (= 2
         (sut/walk-find (sut/saferly even?)
                    {:a 1 :b [2 3]}))))

(deftest merge-recursive-test
  (is (= {:a 2}
         (sut/merge-recursive {:a 1} {:a 2})))
  (is (= {:a {:x 1, :y 11, :z 22}}
         (sut/merge-recursive {:a {:x 1 :y 2}} {:a {:y 11 :z 22}} )))
  (is (= {:a {:x 1 :y 2}}
         (sut/merge-recursive {:a {:x 1 :y 2}} {:a nil} )))
  (is (= 23
         (sut/merge-recursive {:a 1} 23)))
  (is (= {:a #{1 2 10 20}}
         (sut/merge-recursive {:a #{1 2}} {:a #{10 20}})))
  (is (= '{:a [1 2 3 4]}
         (sut/merge-recursive {:a [1 2]} '{:a [3 4]})))
  (is (= '{:a (1 2 3 4)}
         (sut/merge-recursive {:a [1 2]} '{:a (3 4)})))
  )

(deftest union-by-test
  (let [words1 #{"this" "is" "kind" "of" "silly"}
        words2 #{"dont" "you" "think"}
        union (sut/union-by count words1 words2)
        union-counts (set (map count union))]
    ;; the idea is union-by picks one word of each size; not well-defined which one it will be
    (is (= #{2 3 5 4} union-counts))))
    
(deftest safely-test
  (let [double-safe (sut/safely double)]
    (is (= 23.0 (double-safe 23)))
    (is (nil? (double-safe nil)))))

(deftest saferly-test
  (let [even?-safe (sut/saferly even?)]
    (is (= true (even?-safe 10)))
    (is (nil? (even?-safe "estragon")))
    (is (nil? (even?-safe nil)))))

(deftest partition-if-test
  (is (= '((0 1) (2 3) (4 5) (6 7) (8 9))
         (sut/partition-if even? (range 10))))
  (is (= '()
         (sut/partition-if even? '())))
  (is (= '((0))
         (sut/partition-if even? '(0))))
  (is (= '((0 1) (2))
         (sut/partition-if even? (range 3)))))

(deftest partition-diff-test
  (let [x '(1 9 2 0 3 7 2 2 7 2 7 7 5 7 8 6 5 4 2 1)]
    (is (= '((1 9) (2) (0 3 7) (2) (2 7) (2 7) (7) (5 7 8) (6) (5) (4) (2) (1))
           (sut/partition-diff < x)))
    (is (= '((1) (9 2 0) (3) (7 2) (2) (7 2) (7) (7 5) (7) (8 6 5 4 2 1))
           (sut/partition-diff > x)))))

(deftest substitute-test
  (let [vmap {:a :arnold :b :betty :c :claudio}
        struct [:a "likes" #{:b :c}]]
    (= [:arnold "likes" #{:betty :claudio}]
       (sut/substitute struct vmap))))

(deftest substitute-gen-test
  (let [vmap {:a :arnold :b :betty :c :claudio}
        struct [:a "likes" #{:b :c} "likes" :d "hates" :e]
        result (sut/substitute-gen struct vmap gensym)]
    (testing "gen called once per value"
      (is (= (nth result 1) (nth result 3))))
    (testing "gen d values are unique"
      (is (not (= (nth result 3) (nth result 5)))))))

(deftest pam-test
  (is (= (map #(* % 2) (range 10))
         (sut/pam (range 10) #(* % 2)))))

(deftest clean-seq-test
  (is (= '(3 4 [a] "hey")
         (sut/clean-seq '(3 nil 4 "" [] [a] "hey")))))

(deftest clean-map-test
  (is (= {:a 1 :c 3} (sut/clean-map {:a 1 :b nil :c 3})))
  (is (= {} (sut/clean-map {:a nil}))))

(deftest clean-maps-test
  (is (= {:a 1 :c 3} (sut/clean-maps {:a 1 :b nil :c 3})))
  (is (= {} (sut/clean-maps {:a nil})))
  (is (= {:a 1 :b {:x 1} :c 3} (sut/clean-maps {:a 1 :b {:x 1 :y nil} :c 3}))))

(deftest all-keys-test
  (is (= #{:a :b :c :random}
         (sut/all-keys [{:a 1 :b 2} {:a 3 :c 4} {} {:random :bits}]))))

(deftest stratify-test
  (let [results
        (sut/stratify '{1 {:name a :predecessors []}
                    2 {:name b :predecessors [1]}
                    3 {:name c :predecessors [1]}
                    4 {:name d :predecessors [1 3]}
                    }
                  :predecessors :depth)]
    (is (= {1 0 2 1 3 1 4 2} (sut/map-values :depth results))))
  (testing "consistency check"
    (is (thrown? :default
                 (sut/stratify  '{1 {:name a :predecessors []}
                              2 {:name b :predecessors [foo]}
                              3 {:name c :predecessors [1]}}
                            :predecessors :depth)))))

(deftest self-label-test
  (is (= '{1 {:name a :id 1}
           2 {:name b :id 2}}
         (sut/self-label :id '{1 {:name a} 2 {:name b}}))))


(deftest add-inverse-test
  (let [db {:a {:children [:b :c]}
            :b {:children [:d]}}]
    (is (= {:a {:children [:b :c]}
            :b {:children [:d] :parent :a}
            :c {:parent :a} 
            :d {:parent :b}}
           (sut/add-inverse db :children :parent)))))

(deftest add-inverse-multiple-test
  (let [db {:a {:children [:b :c]}
            :b {:children [:c]}}]
    (is (= {:a {:children [:b :c]}
            :b {:children [:c] :parents #{:a}}
            :c {:parents #{:a :b}}}
         (sut/add-inverse-multiple db :children :parents)))))

(deftest fix-test
  (let [base-fib (fn [fib i]
                   (if (< i 2) 1 (+ (fib (- i 1)) (fib (- i 2)))))
        memoized-fib (sut/fix (memoize base-fib))]
    ;; If this wasn't memoized, it would take forever
    (is (= 20365011074 (memoized-fib 50)))))

(deftest duplicates-test
  (is (empty? (sut/duplicates (range 10))))
  (is (= '(1 2) (sut/duplicates '(1 2 3 2 10 1 0 1)))))

  
