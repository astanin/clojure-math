(defproject clojure-math "1.0-SNAPSHOT"
  :description "Incomplete Clojure bindings to Commons Math library"
  :url "http://github.com/astanin/clojure-math"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.apache.commons/commons-math3 "3.2"]
                 [com.googlecode.efficient-java-matrix-library/ejml "0.23"]]
  :profiles {:dev {:dependencies [[criterium "0.3.1"]]
                   :plugins [[codox "0.6.4"] ; lein with-profile dev doc
                             ]}})
