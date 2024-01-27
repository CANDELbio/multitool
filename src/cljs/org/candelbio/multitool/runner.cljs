(ns org.candelbio.multitool.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [org.candelbio.multitool.core-test]
            ))

(doo-tests 'org.candelbio.multitool.core-test
           #_ 'your-project.util-test
           )
