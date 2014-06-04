(defproject automat "0.1.2-SNAPSHOT"
  :description ""
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[rhizome "0.2.1-SNAPSHOT"]
                 [primitive-math "0.1.3"]
                 [potemkin "0.3.4"]
                 [riddley "0.1.7"]
                 [proteus "0.1.4"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [reiddraper/simple-check "0.5.6"]
                                  [criterium "0.4.3"]
                                  [codox-md "0.2.0" :exclusions [org.clojure/clojure]]]}}
  :test-selectors {:default #(every? (complement #{:stress :benchmark}) (keys %))
                   :stress :stress
                   :benchmark :benchmark}
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace ["-server" "-Xmx2g"]
  :java-source-paths ["src"]
  :javac-options ["-target" "1.5" "-source" "1.5"]
  :plugins [[codox "0.6.4"]]
  :codox {:writer codox-md.writer/write-docs
          :include [automat.core automat.viz]})
