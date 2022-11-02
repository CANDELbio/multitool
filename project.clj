(defproject org.parkerici/multitool "0.0.25"
  :description "PICI utility library"
  :url "http://github.com/ParkerICI/multitool"
  :license {:name "MIT License"
             :url "http://opensource.org/licenses/MIT"}
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :source-paths ["src/cljc" "src/clj" "src/cljs"]
  :test-paths ["test/cljc" "test/clj"]  ;TODO cljs testing
  :plugins [[lein-codox "0.10.8"]]
  :jvm-opts ["--add-opens" "java.base/java.lang=ALL-UNNAMED"]
  :codox {:output-path "codox"
          :metadata {:doc/format :markdown}
          :source-uri "http://github.com/CANDELbio/multitool/blob/master/{filepath}#L{line}"}
  )
