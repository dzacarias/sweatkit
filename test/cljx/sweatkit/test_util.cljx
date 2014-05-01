(ns sweatkit.test-util
  #+clj (:require [clojure.java.io :as io]))

#+cljs
(defn test-data [path]
  (aget (this-as this (aget this "cljs_test_data")) path))

(defn read-file [path]
  #+clj (io/file path)
  #+cljs (test-data path))
