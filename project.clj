(defproject automat "0.1.0-SNAPSHOT"
  :description ""
  :license {:name "MIT License"
            :url "http://opensource.org/licenses/MIT"}
  :dependencies [[rhizome "0.1.9"]
                 [primitive-math "0.1.3"]
                 [potemkin "0.3.3"]
                 [proteus "0.1.4"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.5.1"]
                                  [reiddraper/simple-check "0.2.1"]
                                  [criterium "0.4.2"]
                                  [codox-md "0.2.0" :exclusions [org.clojure/clojure]]]}}
  :test-selectors {:default (complement :benchmark)
                   :benchmark :benchmark}
  :global-vars {*warn-on-reflection* true}
  :jvm-opts ^:replace ["-server" "-Xmx2g"]
  :java-source-paths ["src"]
  :javac-options ["-target" "1.5" "-source" "1.5"]
  :plugins [[codox "0.6.4"]]
  :codox {:writer codox-md.writer/write-docs
          :include [automat.core]})
