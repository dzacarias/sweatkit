(defproject sweatkit "0.1.0-SNAPSHOT"
  :description "A Clojure(Script) library to work with sports activities data"
  :url "https://github.com/dzacarias/sweatkit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojurescript "0.0-2202"]
                 [clj-time "0.6.0"]
                 [com.andrewmcveigh/cljs-time "0.1.4"]
                 [prismatic/schema "0.2.1"]]

  :cljx {:builds [{:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :clj}

                  {:source-paths ["src/cljx"]
                   :output-path "target/classes"
                   :rules :cljs}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :clj}

                  {:source-paths ["test/cljx"]
                   :output-path "target/test-classes"
                   :rules :cljs}]}

  :source-paths ["src/clj" "target/classes"]
  :test-paths ["test/clj" "target/test-classes"]
  :jar-exclusions [#"\.cljx|\.DS_Store"]

  :profiles {:dev {:plugins [[com.keminglabs/cljx "0.3.2"]
                             [lein-cljsbuild "1.0.3"]
                             [com.cemerick/clojurescript.test "0.3.0"]
                             [com.cemerick/austin "0.1.4"]
                             [codox "0.8.7"]]
                   :hooks [cljx.hooks]
                   :aliases {"cleantest" ["do" "clean," "cljx" "once," "test," "cljsbuild" "test"]
                             "deploy" ["do" "clean," "cljx" "once," "deploy" "clojars"]}}}

  :cljsbuild { :builds [{:source-paths ["target/classes" "target/test-classes"]
                         :compiler {:output-to "target/js/advanced.js"
                                    :optimizations :advanced
                                    :pretty-print true}}]
              :test-commands {"browser" ["phantomjs" "test-resources/runner.js"
                                         "--test-data=test-resources/tcx"
                                         "this.literal_js_was_evaluated=true"
                                         "target/js/advanced.js"]}}
  :codox {:project {:name "sweatkit"}
          :sources ["target/classes"]
          :exclude [sweatkit.formats.impl.xml]
          :src-dir-uri "https://github.com/dzacarias/sweatkit/blob/master/"
          :src-linenum-anchor-prefix "L"
          :src-uri-mapping {#"target/classes" #(str "src/cljx/" % "x")}})
