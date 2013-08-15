(defproject automat "0.1.0-SNAPSHOT"
  :description ""
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[rhizome "0.1.9"]
                 [primitive-math "0.1.3"]
                 [potemkin "0.3.2"]
                 [proteus "0.1.3-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [reiddraper/simple-check "0.2.1"]]}})
