(ns sweatkit.test-util
  (:require [clojure.java.io :as io]))

(defn read-file [path]
  (io/file path))
