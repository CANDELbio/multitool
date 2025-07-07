(defproject org.candelbio/multitool "0.1.11"
  :description "CANDELBio utility library"
  :url "https://github.com/CANDELbio/multitool"
  :license {:name "Apache 2 License"
            :url "https://opensource.org/licenses/Apache-2.0"}
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/clojurescript "1.12.42"]
                 [net.cgrand/macrovich "0.2.1"]] ;Note: 0.2.2 has a fatal typo?
  :source-paths ["src/cljc" "src/clj" "src/cljs"]
  :test-paths ["test/cljc" "test/clj"]
  :plugins [[lein-codox "0.10.8"]
            [lein-doo "0.1.11"]]
  :jvm-opts ["--add-opens" "java.base/java.lang=ALL-UNNAMED"] ;necessary for codox to run
  :cljsbuild
  {:builds [{:id "test"
             :source-paths ["src" "test/cljs" "test/cljc"]
             :compiler {:output-to "target/testable.js"
                        :optimizations :none
                        :main org.candelbio.multitool.runner
                        :pretty-print true}}]}
  :doo {:build "test"
        :alias {:default [:planck]}}
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          :source-uri "http://github.com/CANDELbio/multitool/blob/master/{filepath}#L{line}"}
  )
