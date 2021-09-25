(defproject org.parkerici/multitool "0.0.17"
  :description "PICI utility library"
  :url "http://github.com/ParkerICI/multitool"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories
  [["github" {:url "https://maven.pkg.github.com/ParkerICI/mvn-packages"
              :sign-releases false
              :username :env/github_user
              :password :env/github_password}]]
  :deploy-repositories [["clojars" {:sign-releases false}]]
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :source-paths ["src/cljc" "src/clj" "src/cljs"]
  :test-paths ["test/cljc" "test/clj"]  ;TODO cljs testing
  )
