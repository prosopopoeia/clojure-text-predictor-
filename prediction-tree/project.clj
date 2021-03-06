(defproject prediction-tree "0.1.0-SNAPSHOT"
  :description "Learning Clojure project on using clojure with java"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot prediction-tree.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
