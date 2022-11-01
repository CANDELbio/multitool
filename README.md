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

# To deploy to Clojars

From a real terminal (not Emacs)

    lein deploy clojars
	
You will need to supply credentials (user name and authentication token).


# License

Relased under MIT license. See the [LICENSE](LICENSE.md) file for details.

