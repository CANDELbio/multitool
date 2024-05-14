(ns org.candelbio.multitool.core
  "Various generally useful utilities"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.walk :as walk])
  #?(:cljs (:require-macros
            [net.cgrand.macrovich :as macros]
            [org.candelbio.multitool.core :refer [doseq* ignore-errors]])
     :clj (:require [net.cgrand.macrovich :as macros]))
  #?(:clj  
     (:import (java.util.regex Pattern)
              (clojure.lang MapEntry))))

;;; Warning: some hacks in here could be considered poor Clojure form. 
;;; TODO Probably time to split this file into multiple

;;; ⩇⩆⩇ Memoization ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See https://github.com/clojure/core.memoize/ for more a elaborate memoization tool.

;;; It would probably be cleaner to use the function metadata, but I want to be able
;;; to clear them all.

(def memoizers (atom {}))

(defn memoize-named
  "Like clojure.core/memoize, but retains a ptr to the cache so it can be cleared (see memoize-reset)"
  [name f]
  (let [mem (atom {})]
    (swap! memoizers assoc name mem)
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn memoize-reset!
  "Clear the cache of one or all memoized fns"
  ([]
   (doseq [[_ mem] @memoizers]
     (reset! mem {})))
  ([name]
   (reset! (get @memoizers name) {})))

(defn memoize-cache
  "Return the cache (map of args to values) for a named memoizer."
  [name]
  (get @memoizers name))

(defn memoizer-stats
  "Return information about all memoize-name fns"
  []
  (zipmap (keys @memoizers)
          (map (comp count deref) (vals @memoizers))))

(macros/deftime
;;; TODO This and its usages needs to be reworked for CLJS (Why? Macros probably)
(defmacro defn-memoized
  "Like `defn`, but produces a memoized function"
  [name args & body]
  ;; This crock is because you can't have multiple varadic arg signatures...sigh
  (if (string? args)
    `(def ~name ~args (memoize-named '~name (fn ~(first body) ~@(rest body))))
    `(def ~name (memoize-named '~name (fn ~args ~@body)))))

(defmacro def-lazy
  "Like `def` but produces a delay; value is acceessed via @ and won't be computed until needed"
  [var & body]
  `(def ~var (delay ~@body)))
)

;;; ⩇⩆⩇ Exception handling ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn throwable?
  [x]
  (instance? x #?(:clj Throwable :cljs js/Error)))

(macros/deftime
(defmacro ignore-errors
  "Execute `body`; if an exception occurs ignore it and return `nil`. Note: strongly deprecated for production code."
  [& body]
  `(try (do ~@body)
        (catch ~(macros/case :clj Throwable :cljs :default) e# nil)))

(defmacro ignore-report
  "Execute `body`, if an exception occurs, print a message and continue"
  [& body]
  `(try (do ~@body)
        (catch #?(:clj Throwable :cljs :default) e#
          (println (str "Ignored: " (str e#))))))

(defmacro ignore-return
  "Execute `body`, if an exception occurs, return it"
  [& body]
  `(try (do ~@body)
        (catch ~(macros/case :clj Throwable :cljs :default) e#
          e#)))
)
(defn error-handling-fn
  "Returns a fn that acts like f, but return value is (true result) or (false errmsg) in the case of an error"
  [f]
  (fn [& args]
    (try
      (let [res (apply f args)]
        (list true res))
      (catch  #?(:clj Throwable :cljs :default) e
        (list false (str "Caught exception: " e))))))

;;; ⩇⩆⩇ Strings ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn truncate-string
  [n s]
  (if (> (count s) n)
    (str (subs s 0 n) "…")
    s))

(defn coerce-numeric
  "Attempt to turn thing into a number (long or double).
  Return number if succesful, otherwise original string"
  [thing]
  (cond
    (number? thing) thing
    (nil? thing) nil
    (and (string? thing) (not (empty? thing)))
    (if-let [inum (re-matches #"-?\d+" thing)]
      (try
        #?(:cljs (js/parseInt ^String inum)
           :clj (Long. ^String inum))
        (catch #?(:clj Throwable :cljs :default)  _ thing))
      (if-let [fnum (re-matches #"-?\d*\.?\d*(E-?\d+)?" thing)]
        (try
          #?(:cljs (js/parseFloat (first fnum))
             :clj (Double. (first fnum)))
          (catch #?(:clj Throwable :cljs "default") _ thing))
        thing))
    :else thing))

(defn coerce-numeric-hard
  "Coerce thing to a number if possible, otherwise return nil"
  [thing]
  (let [n (coerce-numeric thing)]
    (if (number? n) n nil)))

(defn ordinal-suffix
  "The suffix for the ordinal version of n"
  [n]
  (case n
    (11 12 13) "th"                     ;gotta be exceptions
    (case (mod n 10)
      1 "st"
      2 "nd"
      3 "rd"
      "th")))

(defn ordinal
  "Ordinal string for number n, eg 123 → \"123rd\""
  [n]
  (str n (ordinal-suffix n)))

(defn coerce-boolean
  "Coerce a value (eg a string from a web API) into a boolean"
  [v]
  (if (string? v)
    (= v "true")
    (not (nil? v))))

;;; For more conversions like this, use https://clj-commons.org/camel-snake-kebab/
(defn underscore->camelcase
  "Convert foo_bar into fooBar"
  [s]
  (let [parts (str/split s #"_")]
    (apply str (first parts) (map str/capitalize (rest parts)))))

(defn labelize
  "Convert - and _ to spaces"
  [s]
  (str/replace (name s) #"[\_\-]" " "))

;;; TODO this breaks in cljs, can't refer to macro in same file its defined. Argh
#?(:clj 
   (defn-memoized n-chars
     [n char]
     (str/join (repeat n char))))

(defn strip-chars
  "Removes every character of a given set from a string"
  [removed s]
  (reduce str (remove #((set removed) %) s)))

;;; see str/trim, str/triml, str/trimr,
;;; but these let you specify the character set to trim
(defn trim-chars-left
  "Removes every character of a given set from the left end of a string"
  [removed s]
  (let [len (count s)]
    (loop [index 0]
      (if (= len index)
        ""
        (if (contains? removed (.charAt s index))
          (recur (unchecked-inc index))
          (subs s index len))))))

(defn trim-chars-right
  "Removes every character of a given set from the right end of a string"
  [removed s]
  (let [len (count s)]
    (loop [index (- len 1)]
      (if (= len 0)
        ""
        (if (contains? removed (.charAt s index))
          (recur (- index 1))
          (subs s 0 index))))))

(defn trim-chars
  "Removes every character of a given set from the ends of a string"
  [removed s]
  (let [removed (set removed)]
    (->> s
         (trim-chars-left removed)
         (trim-chars-right removed))))

;;; TODO some charsets, esp punc.
(def punc-chars (set "`~!@#$%^&*()_+=-[]\\{}|;'\":/.,<>? "))

(declare clean-seq)
(defn comma-list
  "Splice the non-nullish elements of list together in a string, separated by ', '"
  [list]
  (str/join ", " (clean-seq list)))

;;; ⩇⩆⩇ Regex and templating ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn re-quote
  [s]
  #?(:clj  
     (Pattern/quote s)
     :cljs
     (str/replace s #"([)\/\,\.\,\*\+\?\|\(\)\[\]\{\}\\\$])" "\\$1")))

(defn re-pattern-literal
  "Return a regex that will match the literal string"
  [string]
  (re-pattern (re-quote string)))

;;; TODO make a cljs version (or move to cljcore)
;;; Based on clojure.core/re-seq
#?(:clj 
   (defn re-seq-positions
     "Returns a lazy sequence of successive matches of pattern in string, returning [start end] pairs"
     [^java.util.regex.Pattern re s & [group]]
     (let [m (re-matcher re s)]
       (loop [result []]
         (if (.find m)
           (recur (conj result [(.start m (or group 0)) (.end m (or group 0))]))
           result)))))

;;; TODO make a cljs version (or move to cljcore)
;;; Note: clojure.string/replace is very close to this, but it returns a string instead of fragments.
#?(:clj 
   (defn re-substitute
     "Match re against s, apply subfn to matching substrings. Return list of fragments, processed and otherwise"
     ([re s subfn]
     (let [matcher (re-matcher re s)]
       (loop [fragments ()
              start 0]
         (if (.find matcher)
           (recur (cons (subfn (subs s (.start matcher) (.end matcher)))
                        (cons (subs s start (.start matcher)) fragments))
                  (.end matcher))
           (reverse
            (cons (subs s start) fragments))))))
     ([re s]
      (re-substitute re s identity))))

(defn str-replace-multiple
  [map string]
  (reduce (fn [s [match repl]]
            (str/replace s match repl))
          string map))

(def param-regex #"\{(.*?)\}")          ;extract the template fields from the entity
(def double-braces #"\{\{(.*?)\}\}")
(def javascript-templating #"\$\{(.*?)\}")

;;; Note: default is single braces for parameters {foo}, but :param-regex double-braces option {{foo}} is probably better, works in more contexts.
;;; :param-regex javascript-templating for compatibility with javascript templating ${foo}
(defn expand-template
  "Template is a string containing {foo} elements, which get replaced by corresponding values from bindings. See tests for examples."
  [template bindings & {:keys [param-regex key-fn] :or {param-regex param-regex key-fn identity}}]
  (let [matches (->> (re-seq param-regex template) 
                     (map (fn [[match key]]
                            [match (or (bindings (key-fn key)) "")])))]
    (reduce (fn [s [match key]]
              (str/replace s (re-pattern-literal match) (str key)))
            template matches)))

(def expand-template-string expand-template) ;support old name

(defn validate-template
  "Validate a template, defined as in expand-template-string; fields is a set of allowed field names"
  [template fields & {:keys [param-regex] :or {param-regex param-regex}}]
  (let [vars (map second (re-seq param-regex template))]
    (assert (not (empty? vars)) "Template has no {fields}")
    (doseq [var vars]
      (assert (contains? fields var)
              (str "Template var " var " is not defined")))))

;;; Stolen from clj-glob, where it is internal 
(defn glob->regex
  "Takes a glob-format string and returns an equivalent regex."
  [s]
  (loop [stream s
         re ""
         curly-depth 0]
    (let [[c j] stream]
        (cond
         (nil? c) (re-pattern (str (if (= \. (first s)) "" "(?=[^\\.])") re))
         (= c \\) (recur (nnext stream) (str re c c) curly-depth)
         (= c \/) (recur (next stream) (str re (if (= \. j) c "/(?=[^\\.])"))
                         curly-depth)
         (= c \*) (recur (next stream) (str re "[^/]*") curly-depth)
         (= c \?) (recur (next stream) (str re "[^/]") curly-depth)
         (= c \{) (recur (next stream) (str re \() (inc curly-depth))
         (= c \}) (recur (next stream) (str re \)) (dec curly-depth))
         (and (= c \,) (< 0 curly-depth)) (recur (next stream) (str re \|)
                                                 curly-depth)
         (#{\. \( \) \| \+ \^ \$ \@ \%} c) (recur (next stream) (str re \\ c)
                                                  curly-depth)
         :else (recur (next stream) (str re c) curly-depth)))))


;;; ⩇⩆⩇ Pattern matching ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Ultra-simple pattern matcher.
;;; Something like this must exist? core.match, but not quite
;;; https://github.com/dcolthorp/matchure
;;; TODO multivalent match
(declare lintersection)
(defn pattern-match
  "Ultra-simple structure pattern matcher. Variables are (? <name>), bindings "
  [pat thing]
  (cond (and (list? pat) (= '? (first pat)))
        (if thing
          {(keyword (second pat)) thing}
          nil)
        (and (sequential? pat)
             (sequential? thing)
             (= (count pat) (count thing)))
        (reduce (fn [a b] 
                  (when (and a b
                             (every? (fn [key] (= (a key) (b key)))
                                     (lintersection (keys a) (keys b))))
                    (merge a b)))
                {}
                (map pattern-match pat thing))
        (= pat thing) {}
        :else nil))


;;; ⩇⩆⩇ Keywords, names, namespaces ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn keyword-conc
  "Concatenate parts (which are keywords, strings, anything acceptable to name) into a keyword"
  [& parts]
  (keyword (str/join "-" (map name parts))))

(defn keyword-safe
  "Make a string into a readable keyword by replacing certain punctuation"
  [str]
  (keyword (str/replace str #"[ ,\(\):]" "_")))

(def key-counter (atom {}))

(defn unique-key
  "Produce a unique keyword based on root."
  [root]
  (swap! key-counter update root #(inc (or % 0)))
  (keyword (namespace root)
           (str (name root) (get @key-counter root))))

(defn safe-name
  [thing]
  "name of thing, or nil"
  (when (and thing
             #?(:clj (instance? clojure.lang.Named thing)
                :cljs (.-name thing)))
    (name thing)))

(defn bstr
  "Name or str of thing. Name means 'better str', better in some contexts anyway,"
  [thing]
  (or (safe-name thing)
      (str thing)))

;;; See https://github.com/brandonbloom/backtick for possibly better solution,
(defn de-ns
  "De-namespace. Remove the namespaces that backquote insists on adding. See tests for illustration."
  [struct]
  (walk/postwalk 
   #(if (symbol? %)
    (symbol nil (name %))
    %)
   struct))

;;; ⩇⩆⩇ Variations on standard predicates ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn nullish? 
  "True if value is something we probably don't care about (nil, false, empty seqs, empty strings)"
  [v]
  (or (false? v) (nil? v) (and (seqable? v) (empty? v))))

(defn truthy?
  "Return false if x is nil or false, true otherwise"
  [x]
  (if x true false))

(defn or-nil
  "Given a 1-arg pred, return a new fn that acts as identity if pred is true, nil otherwise"
  [pred]
  (fn [thing] (if (pred thing) thing nil)))

(defn- generalize-comparator
  [comp]
  (fn c ([a b]
         (comp (compare a b) 0))
    ([a b & rest]
     (and (c a b)
          (apply c b rest)))))

(def >* (generalize-comparator >))
(def <* (generalize-comparator <))
(def >=* (generalize-comparator >=))
(def <=* (generalize-comparator <=))
(def =* (generalize-comparator =))      ;not sure this is ever different from =

(defn xor
  "boolean a xor b"
  [a b]
  (or (and a (not b))
      (and b (not a))))

(defn oneof
  "true if exactly one of its arguments is non-nil."
  [& things]
  (= 1 (count (filter identity things))))

;;; ⩇⩆⩇ Sequences ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn real-vector?
  "True if thing is a real vector, not a map entry"
  [thing]
  (and (vector? thing)
       (not (map-entry? thing))))

(defn extend-seq
  "Return a seq padded out to infinity with nils"
  [seq]
  (concat seq (repeat nil)))

;;; I thought split-with did this, but no. Surely this in core?
(defn divide-with
  [p coll]
  (let [groups (group-by (comp true? p) coll)]
    [(get groups true) (get groups false)]))

(defn mapf
  "Like map but filters out nullish? values"
  [f & args]
  (remove nullish? (apply map f args)))

(macros/deftime
(defmacro forf
  "Like for but filters out nullish? values"
  [forms body]
  `(remove nullish?
           (for ~forms
             ~body)))
)

(defn mapcatf
  "Like mapcat but filters out nullish? values"
  [f & args]
  (remove nullish? (apply mapcat f args)))

(defn doall-safe
  "Realize lazy sequences, if arg is such, otherwise acts as identity"
  [thing]
  (if (sequential? thing)
    (doall thing)
    thing))

(defn following-elt
  [elt seq]
  (cond (empty? seq) nil
        (= (first seq) elt) (second seq)
        :else (following-elt elt (rest seq))))

(defn preceding-elt
  [elt seq]
  (cond (empty? seq) nil
        (= (second seq) elt) (first seq)
        :else (preceding-elt elt (rest seq))))

;;; Convention: <f>= names a fn that is like fn but takes an element to test for equality in place of a predicate.
(defn remove= 
  "Remove occurences of elt in seq, applying key-fn before testing if supplied"
  [elt seq & [key-fn]]
  (remove #(= ((or key-fn identity) %) elt) seq))

(defn delete-subseq
  [seq subseq]
  (cond (empty? seq)
        seq
        (= (take (count subseq) seq) subseq)
        (drop (count subseq) seq)
        :else
        (cons (first seq)
              (lazy-seq (delete-subseq (rest seq) subseq)))))

(defn positions
  "Returns a list of indexes of coll for which pred is true"
  [pred coll]
  (keep-indexed (fn [idx x]
                  (when (pred x) idx))
                coll))

(defn position
  "Returns the first index of coll for which pred is true"
  [pred coll]
  (first (positions pred coll)))

(defn positions=
  "Return list of indexes of coll that contain elt"
  [elt coll & [key-fn]]
  (positions #(= ((or key-fn identity) %) elt) coll))

(defn position=
  "Returns the first index of coll that contains elt"
  [elt coll & [key-fn]]
  (first (positions= elt coll (or key-fn identity))))

(defn subseqs
  "Returns a seq of all i-length subseqs"
  [seq i]
  (if (< (count seq) i) '()
      (cons (take i seq)
            (lazy-seq
             (subseqs (rest seq) i)))))

(defn separate
  "Separate coll into two collections based on pred."
  [pred coll]
  (let [grouped (group-by (comp truthy? pred) coll)]
    [(get grouped true) (get grouped false)]))

;;; Formerly threadable
(defn swapped
  "Return a fn like f but with first two arguments swapped"
  [f]
  (fn [a b & rest]
    (apply f b a rest)))

(def ^{:doc "Map backwards. Like map, but takes its args in inverse order. useful in conjunction with ->"}
  pam
  (swapped map))

(defn clean-seq
  "Remove all nullish values from a seq"
  [s]
  (remove nullish? s))

;;; deprecated, use clean-seq with caller doing the filtering
#_
(defn map-filter
  "Applies f to coll. Returns a lazy sequence of the items in coll for which
   all the results that are truthy. f must be free of side-effects."
  [f coll]
  (clean-seq (map f coll)))

(defn some-thing
  "Like some, but returns the original value of the seq rather than the result of the predicate."
  [pred seq]
  (some #(and (pred %) %) seq))

;;; TODO better name for this! Now that it has a much cleaner implementation.
(defn repeat-until
  "Iterate f on start until a value is produced that passes pred, returns value."
  [pred f start]
  (some-thing pred (iterate f start)))

(defn safe-nth
  "Like nth but will return nil if out of bounds rather than erroring"
  [col n]
  (and (< n (count col))
       (>= n 0)
       (nth col n)))

(defn distinctly
  "Like distinct, but equality determined by keyfn"
  [coll keyfn]
  (let [step (fn step [xs seen]
               (lazy-seq
                ((fn [[f :as xs] seen]
                   (when-let [s (seq xs)]
                     (if (contains? seen (keyfn f))
                       (recur (rest s) seen)
                       (cons f (step (rest s) (conj seen (keyfn f)))))))
                 xs seen)))]
    (step coll #{})))

(defn unique-relative-to
  "Generate a name based on s, that is not already found in existing."
  [s existing suffix]
  (if (get (set existing) s)
    (unique-relative-to (str s suffix) existing suffix)
    s))

(defn uncollide
  "Given a seq, return a new seq where the elements are guaranteed unique (relative to key-fn), using new-key-fn to generate new elements"
  [seq & {:keys [key-fn new-key-fn existing] :or {key-fn identity
                                                  new-key-fn #(str % "-1")
                                                  existing #{}
                                                  }}]
  (letfn [(step [xs seen]
            (cond (empty? xs) xs
                  (contains? seen (key-fn (first xs)))
                  (let [new-elt (repeat-until #(not (contains? seen (key-fn %)))
                                              new-key-fn (first xs))]
                    (cons new-elt
                          (step (rest xs) (conj seen (key-fn new-elt)))))
                  :else
                  (cons (first xs)
                        (step (rest xs) (conj seen (key-fn (first xs)))))))]
    (step seq (set existing))))

(defn intercalate
  "Given 2 seqs, produce a seq with alternating elements."
  [l1 l2]
  (cond (empty? l1) l2
        (empty? l2) l1
        :else (cons (first l1)
                    (cons (first l2)
                          (intercalate (rest l1) (rest l2))))))

(defn sequencify
  "Turn thing into a sequence if it already isn't one"
  [thing]
  (when thing
    (if (sequential? thing)
      thing
      (list thing))))

;;; TODO Should be unsequencify for consistency
(defn unlist [thing]
  (if (and (sequential? thing) (= 1 (count thing)))
    (first thing)
    thing))

(defn filter-rest
  "A lazy sequence generated by applying f to seq and its tails"
  [f seq]
  (lazy-seq
   (cond
     (empty? seq) []
     (f seq) (cons seq (filter-rest f (rest seq)))
     :else (filter-rest f (rest seq)))))

;;; TODO these want a version that can accept an alternate for >* or compare
;;; Note: (apply clojure.core/max-key seq) is similar
(defn max-by "Find the maximum element of `seq` based on keyfn"
  [keyfn seq]
  (when-not (empty? seq)
    (reduce (fn [a b] (if (>* (keyfn a) (keyfn b)) a b))
            seq)))

(defn min-by "Find the minimum element of `seq` based on keyfn"
  [keyfn seq]
  (when-not (empty? seq)
    (reduce (fn [a b] (if (<* (keyfn a) (keyfn b)) a b))
            seq)))

;;; Versions of min and max that use generalized compare (and take args in a seq)
(def min* (partial min-by identity))
(def max* (partial max-by identity))

(defn numeric-prefix-sort-key
  "Provide a key for sorting strings with leading numbers"
  [s]
  (let [[_ num rest] (re-matches #"^([0-9]*)(.*)" s)]
    [(when-not (empty? num) (coerce-numeric num)) rest]))

(defn sort-with-numeric-prefix
  "Sort a seq of strings, treating leading numbers in a sane way"
  [seq]
  (sort-by (memoize numeric-prefix-sort-key) seq))

(defn lunion
  "Compute the union of `lists`"
  [& lists]
  (apply set/union (map set lists)))

(defn lintersection
  "Compute the intersection of `lists`"
  [& lists]
  (seq (apply set/intersection (map set lists))))

(defn ldifference                       ;was lset-difference, changed for consistency
  "Compute the set difference of `list1` - `list2'"
  [list1 list2]
  (seq (set/difference (set list1) (set list2))))

;;; partition-lossless
;;; Previously called take-groups
;;; and turns out to be subsumed by clojure.core/partition-all

(defn partition-if
  "Partition coll at every v for which (f v) is true"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [fst (first s)
           run (cons fst (take-while #(not (f %)) (next s)))]
       (cons run (partition-if f (lazy-seq (drop (count run) s))))))))

(defn rest-while
  "Like take-while but applies pred to successive tails of the seq, and returns a seq of tails"
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (when (pred s)
       (cons s (rest-while pred (rest s)))))))

(defn partition-diff
  "Partition coll between v1 and v2 at points for which (f v1 v2) is true"
  [f coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (let [run (rest-while #(and (> (count %) 1)
                                 (f (first %) (second %)))
                           s)
           psize (+ 1 (count run))]
       (cons (take psize s)
             (partition-diff f (drop psize s)))))))

;;; Clump-by deprecated and removed when I realized it was basically identical to clojure.core/partition-by

(defn map-chunked
  "Call f with chunk-sized subsequences of l, concat the results"
  [f chunk-size l]
  (mapcat f (partition-all chunk-size l)))

(macros/deftime
(defmacro doseq*
  "Like doseq, but goes down lists in parallel rather than nested. Assumes lists are same size."
  {:style/indent 1}
  [bindings & body]
  (let [bindings (partition 2 bindings)
        vars (map first bindings)
        forms (map second bindings)
        lvars (map gensym vars)]
    `(loop ~(into [] (mapcat (fn [v f] [v f]) lvars forms))
       (let ~(into [] (mapcat (fn [v lv] [v `(first ~lv)]) vars lvars))
         ~@body
         (when-not (empty? (rest ~(first lvars)))
           (recur ~@(map (fn [lv] `(rest ~lv)) lvars)))
         ))))

(defmacro for*
  "Like for but goes down lists in parallel rather than nested. Assumes lists are same size."
  {:style/indent 1}
  [bindings & body]
  (let [bindings (partition 2 bindings)
        vars (map first bindings)
        forms (map second bindings)]
    `(map (fn ~(into [] vars) ~@body) ~@forms)))

;;; TODO figure out way to make emacs indent work
(defmacro forcat
  [vars body]
  `(apply concat (for ~vars ~body)))
)

;;; ⩇⩆⩇ Vectors ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn lconj
  "Conj a value to the front (left) of vector. Not performant"
  [v e]
  (vec (cons e v)))

;;; ⩇⩆⩇ Maps ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn all-keys
  "Given a seq of maps, return the union of all keys"
  [sheet-data]
  (reduce set/union (map (comp set keys) sheet-data)))


;;; Note:
;;;   Changed in 0.0.12 to not error if unmergeable
;;;   Changed in 0.0.19 to merge terminal sets and sequences
;;; TODO should take arbitary # of args like merge and merge-with
(defn merge-recursive-with
  "Recursively merge two arbitrariy nested map structures, merging terminals (non-maps) with f"
  [f m1 m2]
  (cond (and (map? m1) (map? m2))
        (merge-with (partial merge-recursive-with f) m1 m2)
        (nil? m2) m1
        (nil? m1) m2
        :else (f m1 m2)))

(defn merge-recursive
  "Recursively merge two arbitrariy nested map structures. Terminal seqs are concatentated, terminal sets are merged."
  [m1 m2]
  (merge-recursive-with
   (fn [v1 v2]
     (cond (nil? v1) v2
           (nil? v2) v1
           (and (set? v1) (set? v2))
           (set/union v1 v2)
           (and (vector? v1) (vector? v2))
           (into [] (concat v1 v2))
           (and (sequential? v1) (sequential? v2))
           (concat v1 v2)
           :else v2)                    ;Thought about returning a pair in this case but that seems a bit off.
     )
   m1 m2))

;;; The fns below are going to be in clojure.core someday
;;; https://clojure.atlassian.net/browse/CLJ-1959

;;; For Clojure, see cljcore/pmap-values cljcore/pmap-keys

(defn map-values
  "Map f over the values of hashmap"
  [f hashmap]
  (reduce-kv (fn [acc k v] (assoc acc k (f v))) {} hashmap))

(defn map-keys
  "Map f over the keys of hashmap"
  [f hashmap]
  (reduce-kv (fn [acc k v] (assoc acc (f k) v)) {} hashmap))

(defn map-keys-recursive
  "Map f over the keys of hashmap, and any nested hashmaps"
  [f hashmap]
  (if (map? hashmap)
    (reduce-kv (fn [acc k v] (assoc acc (f k) (map-keys-recursive f v))) {} hashmap)
    hashmap))

(defn dehumanize
  "Convert string keys to keywords, recursively"
  [map]
  (map-keys-recursive (comp keyword-safe str/lower-case) map))

(defn map-key-values
  "Map f over [k v] of hashmap, returning new v"
  [f hashmap]
  (reduce-kv (fn [acc k v] (assoc acc k (f k v))) {} hashmap))

;;; TODO map-values-recursive, map-key-values-recursive

(defn index-by 
  "Return a map of the elements of coll indexed by (f elt). Similar to group-by, but overwrites elts with same index rather than producing vectors. "
  [f coll]  
  (zipmap (map f coll) coll))

(defn distinct*?
  "Given a seq, return true if all elements are distinct. See distinct?"
  [seq]
  (= (count seq) (count (distinct seq))))

(defn duplicates
  "Return elements that occur more than once."
  [seq]
  (map first (filter #(> (second %) 1) (frequencies seq))))

;;; TODO tests
(defn index-by-safely
  "Return a map of the elements of coll indexed by (f elt). Throw an exception of there are duplicate keys."
  [f coll]  
  (let [keys (map f coll)]
    (when-not (distinct*? keys)
      (throw (ex-info "Duplicate keys in index-by-safely" {:dupes (duplicates keys)})))
    (zipmap keys coll)))

;;; TODO this is confusingly named; there are several kinds of multiple to deal with.
;;; TODO Also no tests, and also should use transients
(defn index-by-multiple
  "Like index-by, but f produces a seq of values rather than a single one"
  [f coll]
  (reduce
   (fn [ret x]
     (reduce (fn [ret y]
               (assoc ret y x))
             ret (f x)))
   {} coll))

(defn index-by-ordered 
  "Return an array map of the elements of coll indexed by (f elt), preserving the order. See index-by"
  [f coll]  
  (apply array-map
         (mapcat (fn [elt] [(f elt) elt]) coll)))

(defn union-by
  "Return unique elements in (union s1 s2) given f as identity key"
  [f s1 s2]
  (set (vals (merge (index-by f s1) (index-by f s2)))))

(defn index-by-and-transform
  "Return a map of the elements of coll indexed by (f elt) and transformed by (g elt). "
  [f g coll]  
  (zipmap (map f coll) (map g coll)))

;;; TODO use transients as in group-by
;;; wait isn't this the same as index-by-multiple??? Argh.
(defn group-by-multiple
  "Like group-by, but f produces a seq of values rather than a single one; the orginal value gets grouped with each of them"
  [f coll]  
  (reduce
   (fn [ret x]
     (reduce (fn [ret y]
               (assoc ret y (conj (get ret y []) x)))
             ret (f x)))
   {} coll))

;;; deprecated, makes more sense for caller to do whatever transformations are needed
(defn group-by-and-transform
  "Like group-by, but the values of the resultant map have f mapped over them"
  [by f seq]
  (map-values
   #(map f %)
   (group-by by seq)))

(defn dissoc-if
  [f hashmap]
  (apply dissoc hashmap (map first (filter f hashmap))))

(defn dissoc-in
  "Dissoc in a nested map structure"
  [map [k & k-rest]]
  (if k-rest
    (update map k dissoc-in k-rest)
    (dissoc map k)))

(defn merge-in
  "Merge in a nested map structure"
  [map [k & k-rest] changes]
  (if k-rest
    (update map k merge-in k-rest changes)
    (update map k merge changes)))

;;; Deprecated, use clean-map
(defn remove-nil-values
  [hashmap]
  (dissoc-if (fn [[_ v]] (not v)) hashmap))

(defn map-invert-multiple
  "Returns the inverse of map with the vals mapped to the keys. Like set/map-invert, but does the sensible thing with multiple values.
Ex: `(map-invert-multiple  {:a 1, :b 2, :c [3 4], :d 3}) ==>⇒ {2 #{:b}, 4 #{:c}, 3 #{:c :d}, 1 #{:a}}`"
  [m]
  (map-values
   (partial into #{})
   (reduce (fn [m [k v]]
            (reduce (fn [mm elt]
                      (assoc mm elt (cons k (get mm elt))))
                    m
                    (sequencify v)))
          {}
          m)))

(defn clean-map
  "Remove values from 'map' based on applying 'pred' to value (default is `nullish?`). "
  ([map] (clean-map map nullish?))
  ([map pred] (select-keys map (for [[k v] map :when (not (pred v))] k))))

(defn clean-maps
  "Remove values recursively from 'map' based on applying 'pred' to value (default is `nullish?`). "
  ([map] (clean-maps nullish? map))
  ([pred map] (if (map? map)
                (map-values
                 (partial clean-maps pred)
                 (clean-map map pred))
                map)))

;;; See also clojure.data/diff
;;; The result of this is way too verbose
;;; TODO generalize to >2 args
(defn map-diff
  "Returns a recursive diff of two maps, which you will want to prettyprint."
  [a b]
  (let [both (set/intersection (set (keys a)) (set (keys b)))
        a-only (set/difference (set (keys a)) both)
        b-only (set/difference (set (keys b)) both)
        slot-diffs
        (for [k both
              :let [av (get a k)
                    bv (get b k)]
              :when (not (= av bv))]
          (if (and (map? av) (map? bv))
            [:slot-diff k (map-diff av bv)]
            [:slot-diff k av bv]))]
    [:a-only a-only :b-only b-only :slot-diffs slot-diffs]))

(defn sort-map-by-values [m]
  (into
   (sorted-map-by
    (fn [k1 k2]
      (compare [(get m k1) k1]
               [(get m k2) k2])))
   m))

(defn sort-map-by-values-fn [f m]
  (into
   (sorted-map-by
    (fn [k1 k2]
      (compare [(f (get m k1)) k1]
               [(f (get m k2)) k2])))
   m))

(defn freq-map [seq]
  (sort-map-by-values (frequencies seq)))

;;; ⩇⩆⩇ Transients ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;;  unaccountably missing from core
(defn update!
  [map k f & args]
  (assoc! map k (apply f (get map k) args)))

;;; ⩇⩆⩇ Collecters ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Warning: these work via side-effects so beware of lazy seq evaluation;
;;; You probably need to use deseq instead of for/map

(defn make-collecter
  [init collect-fn]
  (fn [exec]
    (let [acc (atom init)
          collect #(swap! acc collect-fn %)]
      (exec collect)
      @acc)))

(def collecting
  "Exec is a fn of one argument, which is called and passed another fn it can use to collect values; the collection is returned. See tests for example"
  (make-collecter [] conj))

(def collecting-merge
  "Exec is a fn of one argument, which is called and passed another fn it can use to collect values which are merged with merge-recursive; the result is returned. See tests for example TODO" 
  (make-collecter {} merge-recursive))

;;; ⩇⩆⩇ Walkers ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; Some common patterns

(defn walk-filtered
  "Walk thing, applying f to every object that passes filter"
  [f thing filter]
  (walk/postwalk
   (fn [thing]
     (if (filter thing)
       (f thing)
       thing))
   thing))

(defn walk-map-entries
  "Walk all the map entries in thing"
  [f thing]
  (walk-filtered f thing map-entry?))

;;; Unaccountably not in the language
#?(:clj
   (defn map-entry
     "Make a map entry"
     ([k v]
      (MapEntry/create k v))
     ([[k v]]
      (map-entry k v)))
   )

(defn walk-keys
  "Walk all the map entries in thing matching key (a key or set)"
  [f keys thing]
  (let [keys (set (if (coll? keys) keys [keys]))]
    (walk-filtered f thing #(and (map-entry? %) (keys (first %))))))

;;; Previously subst, but that collides with clojure.core
(defn substitute
  "vmap defines a substitution; Walk `struct`, replacing any keys that appear in vmap with corresponding value."
  [struct vmap]
  (walk/postwalk #(if (contains? vmap %) (vmap %) %) struct))

(defn substitute-gen
  "Like `substitute`, but for any terminal elements not in map, call `generator` on first occurance to generate a value."
  [struct vmap generator]
  (let [cache (atom vmap)
        generate (fn [k]
                   (let [v (generator k)]
                     (swap! cache assoc k v)
                     v))]
    (walk/postwalk #(cond (coll? %) %
                          (contains? @cache %) (@cache %)
                          :else (generate %))
                   struct)))

;;; TODO needs tests/examples
(def ^:dynamic *side-walk-context* ())

(defn side-walk
  "Walks form, an arbitrary data structure, evaluating f on each element for side effects. Note: has nothing to do with the standard (functional) walker, and maybe should have a different name (traverse?)"
  [f form]
  (do 
    (f form)
    (binding [*side-walk-context* (cons form *side-walk-context*)]
      (cond
        (coll? form) (doseq [elt form] (side-walk f elt))
        (map-entry? form)
        (do (side-walk f (key form))
            (side-walk f (val form)))))))

;;; Formerly side-reduce
(defn walk-reduce
  "Walks form with an accumulator. f is a function of [accumulator elt], init is initial val of accumulator."
  [f form init]
  (let [acc (atom init)]          ;typically acc should be transient, but since they need special mutators can't be done in a general way. See walk-collect below
    (side-walk
     (fn [elt]
       (swap! acc f elt))
     form)
    @acc))

(defn walk-collect
  "Walk f over thing and return a list of the non-nil returned values"
  [f thing]
  (persistent!
   (walk-reduce (fn [acc elt]
                 (if-let [it (f elt)]
                   (conj! acc it)
                   acc))
               thing
               (transient []))))

(defn walk-find
  "Walk over thing and return the first val for which f is non-nil"
  [f thing]
  (try
    (side-walk
     (fn [thing]
       (when (f thing)
         (throw (ex-info "value" {:value thing}))))
     thing)
    nil
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs ExceptionInfo)
              e
      (:value (ex-data e)))))

(defn clean-walk
  "Remove values from all maps in 'struct' based on 'pred' (default is `nullish?`). "
  ([struct] (clean-walk struct nullish?))
  ([struct pred] 
   (walk/postwalk #(if (map? %) (clean-map % pred) %) struct)))

(defn dissoc-walk
  [struct & keys]
  (walk/postwalk #(if (map? %)
                    (apply dissoc % keys)
                    %)
                 struct))

(macros/usetime
(defn side-walk-paths
  ([f form path]
   (do 
     (f form path)
     (cond
       (map? form)
       (doseq [k (keys form)]
         (side-walk-paths f (get form k) (conj path k)))
       (sequential? form)
       (doseq* [elt form i (range)]
         (side-walk-paths f elt (conj path i))))))
  ([f form]
   (side-walk-paths f form [])))

(defn side-walk-find-paths
  [pred form]
  (collecting
   (fn [collector]
     (side-walk-paths
      (fn [thing path]
        (when (pred thing)
          (collector path)))
      form))))
)

(comment
  (def x {:a 1 :b {:c 2 :d 3 :e [1 7 2 4] :f [{:x 1} {:y 2}]}})
  (side-walk-find-paths (fn [thing] (and (number? thing) (even? thing))) x)

  [[:b :c] [:b :e 2] [:b :e 3] [:b :f 1 :y]]
  )


;;; ⩇⩆⩇ Sets and Bags ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

(defn set=
  "True if c1 and c2 are equal considered as sets"
  [c1 c2]
  (= (set c1) (set c2)))

(defn bag=
  "True if c1 and c2 are equal considered as bags"
  [c1 c2]
  (and (= (count c1) (count c2))
       (set= c1 c2)))

(defn sconj
  "Like conj but will always create a set."
  [coll elt]
  (conj (set coll) elt))

(defn powerset
  "Compute the powerset of a set"
  [s]
  (if (empty? s) #{#{}}
      (let [elt (set (list (first s)))
            tail (powerset (rest s))]
        (set/union (into #{} (map #(set/union elt %) tail))
                   tail))))

;;; ⩇⩆⩇ Functional ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; See tests for an example
(defn fix
  "Fixed-point combinator, useful in conjunction with memoization"
  [f]
  (fn g [& args] (apply f g args)))

;;; Use fixed-point combinator to memoize a recursive function
;;; Stolen from https://stackoverflow.com/questions/27445876/is-there-a-simpler-way-to-memoize-a-recursive-let-fn
(macros/deftime
(defmacro memoize-rec
  [form]
  (let [[fn* fname params & body] form
        params-with-fname (vec (cons fname params))]
    `(let [f# (memoize (fn ~params-with-fname
                         (let [~fname (partial ~fname ~fname)] ~@body)))]
       (partial f# f#)))))

;;; Already in clojure, but I like this name better.
;;; (defaulted f x) returns a new fn that will substitute x for nil args (f is called on x)
(def defaulted fnil)

(defn safely
  "Given f, produce new function that permits nulling."
  [f]
  (fn [x] (and x (f x))))

(defn saferly
  "Given f, produce new function that will return nils if exception is thrown. Not recommended for production code"
  [f]
  (fn [x] (ignore-errors (f x))))

(defn transitive-closure 
  "f is a fn of one arg that returns a list. Returns a new fn that computes the transitive closure of f."
  [f]
  (fn [root]
    (loop [done (set nil)
           fringe (list root)]
      (if (empty? fringe)
        done
        (let [expanded (first fringe)
              expansion (f expanded)
              new (set/difference (set expansion) done)]
          (recur (set/union done (set (list expanded)))
                 (concat new (rest fringe))))))))

;;; Vectorized fns 

(defn vectorize
  "Given a fn f with scalar args, (vectorized f) is a fn that takes either scalars or vectors for any argument,
  doing the appropriate vectorization. All vector args should be the same length."
  [f]
  (fn [& args]
    (if-let [l (some #(and (sequential? %) (count %)) args)]
      (let [vargs (map #(if (sequential? %) (vec %) %) args)] ;vectorize args
        (mapv (fn [i]
               (apply f (map (fn [arg] (if (vector? arg) (get arg i) arg)) vargs)))
             (range l)))
      (apply f args))))

;;; TODO vectorised functions (+*, -* etc) for all basic arith

;;; ⩇⩆⩇ Randomness, basic numerics ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; (things that are more for stats or geometry are in org.candelbio.multitool.math)

(defn rand-range
  "Return a random float between a and b"
  [a b]
  (+ a (* (rand) (- b a))))

(defn rand-range-int
  "Return a random int between a and b"
  [a b]
  (+ a (rand-int (- b a))))

(defn rand-around
  "Return a random float within range of p"
  [p range]
  (rand-range (- p range) (+ p range)))

(defn random-element
  "Return a random element from a seq"
  [seq]
  (nth seq (rand-int (count seq))))

(defn random-elements
  "Return n random elements from a seq"
  [n seq]
  (repeatedly n #(random-element seq)))

(defn round
  "Round the argument"
  [n]
  (if (int? n) n (Math/round n)))

(defn range-truncate
  "Return the closest value to v within range [lower, upper]"
  [v lower upper]
  (and (number? v)
       (max lower (min upper v))))

(defn hex-string
  "Output number as hex string"
  [n]
  #?(:clj (format "%x" n)
     :cljs (.toString n 16)))

;;; ⩇⩆⩇ Graph computations ⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇⩆⩇

;;; TODO radically inefficient for high n, needs memoization
;;; TODO option for n = infinity, that is transitive closure. In fact TODO integrate with transitive-closure
(defn neighborhood
  "Computes the neighborhood of radius n from from, neighbors is a function that produces the immediate neighbors"
  [from n neighbors]
  (if (zero? n)
    (set (list from))
    (set (cons from (mapcat #(neighborhood % (- n 1) neighbors)
                            (neighbors from))))))

(defn stratify
  "g is a map, predecessors is a function of g values to g indices.
  computes for each node the depth: if no predecssors 0, otherwise (inc (max (depth predecssors)))
  useful for laying out DAGs, possibly elsewhere"
  [g predecessors depth-prop]
  (letfn [(depth [node-id]
            (assert (contains? g node-id)) ;sanity check
            (let [predecessors (predecessors (get g node-id))]
              (if (empty? predecessors)
                0
                (+ 1 (apply max (map depth predecessors))))))]
    ;; TODO depth should be memoized, but that is tricky
    ;; see https://blog.klipse.tech/lambda/2016/08/10/y-combinator-app.html
    ;; Which really should be in this library anyway...
    (map-key-values (fn [k v] (assoc v depth-prop (depth k))) g)))

(defn- map-db-children
  [f db children-att]
  (reduce-kv (fn [acc key item]
               (reduce (fn [acc child]
                         (f acc key child))
                       acc
                       (children-att item)))
             db
             db))

;;; AKA add-parent
(defn add-inverse
  "Given a db (map of maps), and a multi-valued attribute children-att, compute the single-valued inverse relationship as parent-att"
  [db children-att parent-att]
  (map-db-children
   (fn [db key child]
     (assoc-in db [child parent-att] key))
   db
   children-att))

;;; AKA add-parents
(defn add-inverse-multiple
  "Given a db (map of maps), and a multi-valued attribute children-att, compute the single-valued inverse relationship as parent-att"
  [db children-att parent-att]
  (map-db-children
   (fn [db key child]
     (update-in db [child parent-att] sconj key))
   db
   children-att))
  
(defn self-label
  "Given a map HASHMAP with maps as values, adds the index to each value as the value of attriute ATTR"
  [attr hashmap]
  (map-key-values (fn [k v] (assoc v attr k)) hashmap))

