# multitool

A collection of handy Clojure utilities and language extensions.

Aka [trashy little subroutines](https://github.com/chrislgarry/Apollo-11/blob/422050965990dfa8ad1ffe4ae92e793d7d1ddae5/Luminary099/LUNAR_LANDING_GUIDANCE_EQUATIONS.agc#L1375). 

This has managed to avoid having any dependencies except for those bundled with Clojure. That may change in the future. 



![Multitool](https://ae01.alicdn.com/kf/HTB1Z4FMaOLxK1Rjy0Ffq6zYdVXaA/2019-New-Design-Multi-Tools-Plier-Folding-Knife-Survival-Multitool-Outdoor-EDC-Gear-Camping-Fishing-Tool.jpg)

# Contents

- `core.cljc`
 Simple, general abstractions. 
- `cljcore.clj`
 Same, but specific to Java environment
 - `math.cljc`
 Simple math and statistics fns
 - `nlp.cljc`
 simple string tokenization

# See also

https://github.com/weavejester/medley


# To deploy to mvn-packages

Bump version, and 

    lein deploy github

# To deply to Clojars

From a real terminal (not Emacs)

    lein deploy clojars
	
You will need to supply credentials (user name and authentication token).


# Release Notes

## 0.0.23

optional 2nd arg for local-file
leading-numeral-key → numeric-prefix-sort-key
add some indent info
fix cljs issue with walk-fin
add fix (fixed-point combinator)
fix math/interpolated to work as one would expect
added math/divides? and math/prime?
made math/primes to be much more efficient


## 0.0.20 - 0.0.22,

add pattern-match and misc
aadded ignore-return; fix a bug in prime-factors (affected only performance, not accuracy)
remove cljcore/random-uuid, now part of clojure.core (as of cclojure 1.11)


## 0.0.19,

fix typo, include norvig word file utils
add memoize-cache
tweak merge recursive to concat terminal seqs; add collecting-merge
split browser code from cljscore
add set unioning to merge-recursive

## 0.0.18

- added: ordinal-suffix, n-chars, index-by-multiple, update!, collecting, add-inverse[-multiple], collecting
- added: neighborhood, add-inverse, add-inverse-multiple
- added copy-paths, download (clj only)
- renamed threadable to swapped
- fixed bugs in safe-nth, sequencify


## 0.0.17

- add threadable and make pam use it
- add merge-in
- add map-key-values and stratify
- add self-label
  
## 0.0.16

- random doc improvements
- rename memoize-reset → memoize-reset! 
- add memoizer-stats
- add pmap-values and pmap-keys
- renames subst → substitute, to avoid collision with clojure.core

## 0.0.15

- added resettable memoizers
- added walk-collect and other side-effecting structure walker utilities
- added trim-chars and other string-cleaning utilities
- added neighborhood
- changes to merge-recursive to be less throw-prone
- added partition-diff
- added comma-list
- assorted bug-fixes, cleanups, tests
