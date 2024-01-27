# multitool

A collection of handy Clojure utilities and language extensions.

Aka [trashy little subroutines](https://github.com/chrislgarry/Apollo-11/blob/422050965990dfa8ad1ffe4ae92e793d7d1ddae5/Luminary099/LUNAR_LANDING_GUIDANCE_EQUATIONS.agc#L1375). 

[API Documentation](https://candelbio.github.io/multitool)


This has managed to avoid having (almost) any dependencies except for Clojure itself. 


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

# To deploy to Clojars

From a real terminal (not Emacs)

    lein deploy clojars
	
You will need to supply credentials (user name and authentication token).


# To build documentation

    lein codox
    TODO push to github has an action which publishes but is not yet smart enough to run this first. 
	This https://docs.github.com/en/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site#publishing-with-a-custom-github-actions-workflow might help

# License

Released under Apache 2 license. See the [LICENSE](LICENSE.md) file for details.

