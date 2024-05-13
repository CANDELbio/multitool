(ns org.candelbio.multitool.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [org.candelbio.multitool.core-test]
            [org.candelbio.multitool.nlp-test]
            [org.candelbio.multitool.math-test]
            ))

;;; This is the running for clojurescript tests (and maybe should be a cljs file, but works here)

(doo-tests 'org.candelbio.multitool.core-test
           'org.candelbio.multitool.nlp-test
           'org.candelbio.multitool.math-test
           #_ 'your-project.util-test
           )
