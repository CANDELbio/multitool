(defproject multitool "0.1.5"
  :description "PICI utility library"
  :url "http://github.com/ParkerICI/multitool"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"]
                 ]
  :source-paths ["src/cljc" "src/clj"] 
  :test-paths ["test/cljc" "test/clj"]

  :plugins [[lein-cljsbuild "1.1.7"]]
  :cljsbuild
  {:builds [{:source-paths ["src/cljc" "src/cljs"]
             :compiler {:optimizations :none
                        :pretty-print true}}]}
  )
