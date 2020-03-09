(defproject multitool "0.1.5"
  :description "PICI utility library"
  :url "http://github.com/ParkerICI/multitool"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories 
  [["github" {:url "https://maven.pkg.github.com/ParkerICI/mvn-packages"
              :sign-releases false
              :username :env/github_user
              :password :env/github_password}]]
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/clojurescript "1.10.520"]]
  :source-paths ["src/cljc" "src/clj"] 
  :test-paths ["test/cljc" "test/clj"]
  )
