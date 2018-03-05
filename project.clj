(defproject ulmus "0.1.0-SNAPSHOT"
  :description "Reactive programming implemented as a kinder layer over core.async."
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.946"]
                 [org.clojure/core.async "0.3.443"]]

  :plugins [[lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]
            [lein-codox "0.10.3"]]

  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :compiler {:output-to "target/js/ulmus.js"
                           :output-dir "target/js/out"}}
               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "target/js/ulmus.js"
                           :optimizations "advanced"}}]}
  :codox {:output-path "docs"
          :language :clojurescript
          :metadata {:doc/format :markdown}})

