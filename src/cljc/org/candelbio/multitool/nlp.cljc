(ns org.candelbio.multitool.nlp
  (:require [clojure.string :as str]
            [org.candelbio.multitool.core :as core]))

;;; Source: http://rosettacode.org/wiki/Levenshtein_distance#Clojure
;;; can be extremely slow eg (levenshtein "restaurant" "restoration")
(defn levenshtein [str1 str2]
  (let [len1 (count str1)
        len2 (count str2)]
    (cond (zero? len1) len2
          (zero? len2) len1
          :else
          (let [cost (if (= (first str1) (first str2)) 0 1)]
            (min (inc (levenshtein (rest str1) str2))
                 (inc (levenshtein str1 (rest str2)))
                 (+ cost
                    (levenshtein (rest str1) (rest str2))))))))

;;; lowercase and tokenize a string. Punctuation is removed (except for ').
;;; \p{L} means match any char of any language.
;;; TODO doesn't work at all in cljs
;;; TODO should have a version that skips lower-casing
(defn tokens
  [s]
  (map str/lower-case 
       (re-seq #"[\p{L}'\d]+" s)))

(defn bigrams
  [tokens]
  (map list tokens (rest tokens)))

(def stops
  #{"a" "about" "above" "after" "again" "against" "all" "am" "an" "and" "any" "are" "aren't" "as" "at" "be" "because" "been" "before" "being" "below" "between" "both" "but" "by" "can" "can't" "cannot" "could" "couldn't" "did" "didn't" "do" "does" "doesn't" "doing" "don't" "down" "during" "each" "few" "for" "from" "further" "had" "hadn't" "has" "hasn't" "have" "haven't" "having" "he" "he'd" "he'll" "he's" "her" "here" "here's" "hers" "herself" "him" "himself" "his" "how" "how's" "i" "i'd" "i'll" "i'm" "i've" "if" "in" "into" "is" "isn't" "it" "it's" "its" "itself" "let's" "me" "more" "most" "mustn't" "my" "myself" "no" "nor" "not" "of" "off" "on" "once" "only" "or" "other" "ought" "our" "ours" "ourselves" "out" "over" "own" "same" "shan't" "she" "she'd" "she'll" "she's" "should" "shouldn't" "so" "some" "such" "than" "that" "that's" "the" "their" "theirs" "them" "themselves" "then" "there" "there's" "these" "they" "they'd" "they'll" "they're" "they've" "this" "those" "through" "to" "too" "under" "until" "up" "very" "was" "wasn't" "we" "we'd" "we'll" "we're" "we've" "were" "weren't" "what" "what's" "when" "when's" "where" "where's" "which" "while" "who" "who's" "whom" "why" "why's" "with" "won't" "would" "wouldn't" "you" "you'd" "you'll" "you're" "you've" "your" "yours" "yourself" "yourselves"})

(defn remove-stops
  [tokenized stops]
  (remove #(get stops %) tokenized))

(defn remove-numbers
  [tokenized]
  (remove (partial re-matches #"[\d\-]+") tokenized))

(defn remove-shorts
  [tokenized]
  (remove #(< (count %) 3) tokenized))

(defn remove-ruthlessly
  [tokenized]
  (-> tokenized
      remove-shorts
      (remove-stops stops)
      remove-numbers))




;;; Example of use

#_
(defn file-frequencies [f]
  (-> f
      slurp
      tokens
      (remove-stops stops)
      frequencies
      core/sort-map-by-values))


(defn overexpressed
  [freq base-freq]
  (core/sort-map-by-values
   (reduce (fn [m [k v]]
             (if-let [base (get base-freq k)]
               (assoc m k (/ v base))
               m)) {} freq)))


