(defproject sweatkit "0.1.4"
  :description "A Clojure library to work with sports activities data"
  :url "https://github.com/dzacarias/sweatkit"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :scm {:name "git"
        :url "https://github.com/dzacarias/sweatkit"}

  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/data.zip "0.1.1"]
                 [org.clojure/data.xml "0.0.8"]
                 [clj-time "0.9.0"]
                 [prismatic/schema "0.3.3"]]
  
  :jar-exclusions [#"\.DS_Store"]

  :profiles {:dev {:plugins [[codox "0.8.9"]]}}

  :codox {:project {:name "sweatkit"}
          :exclude [sweatkit.formats.impl.xml]
          :src-dir-uri "https://github.com/dzacarias/sweatkit/blob/master/"
          :src-linenum-anchor-prefix "L"})
