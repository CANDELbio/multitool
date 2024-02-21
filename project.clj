(defproject org.candelbio/multitool "0.1.4"
  :description "CANDELBio utility library"
  :url "https://github.com/CANDELbio/multitool"
  :license {:name "Apache 2 License"
             :url "https://opensource.org/licenses/Apache-2.0"}
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :source-paths ["src/cljc" "src/clj" "src/cljs"]
  :test-paths ["test/cljc" "test/clj"]  ;TODO cljs testing
  :plugins [[lein-codox "0.10.8"]]
  :jvm-opts ["--add-opens" "java.base/java.lang=ALL-UNNAMED"] ;necessary for codox to run
  :codox {:output-path "docs"
          :metadata {:doc/format :markdown}
          :source-uri "http://github.com/CANDELbio/multitool/blob/master/{filepath}#L{line}"}
  )
