(ns sweatkit.formats.tcx-test
  (:require #+clj  [clojure.test :as t :refer :all]
            #+cljs [cemerick.cljs.test :as t]
            #+clj  [clojure.java.io :as io]
            #+clj  [clj-time.coerce :as tc]
            #+cljs [cljs-time.coerce :as tc]
            [sweatkit.formats.tcx :refer (parse)])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))

#+cljs
(defn- lookup-file-str [path]
   ;(this-as this (aget this "prop_name"))
  )

(defn- parse-file [path]
  (let [f #+clj (io/file path)
          #+cljs (lookup-file-str path)]
    (parse f)))

;; -----------------------------------------------------------------------------
;; Unit tests
;; Some example-based unit tests, to cover the basics

#+clj
(deftest activity-detail-test
  (testing "Parsing activity detail with multiple metrics, including GPS"
    (let [p (parse-file "test-resources/FitnessHistoryDetail.tcx")
          a (first (:activities p))
          s (first (:segments a))]
      
      (is (= 1 (count (:activities p))))
      (is (= (tc/from-string "2007-08-07T02:42:41Z") (:dtstart a)))
      (is (= (sort (keys (:metrics s)))
             (sort [:distance :speed :calories :position :altitude])))
      (is (= 285 (get-in s [:metrics :calories :total])))
      (is (= 8348.5039063 (get-in s [:metrics :distance :total])))
      (is (= 18.6828499 (get-in s [:metrics :speed :max])))
      (is (:active s))
      (is (= :manual (:trigger s)))
      (is (= 373 (count (get-in s [:metrics :distance :track]))))
      (is (= 373 (count (get-in s [:metrics :altitude :track]))))
      (is (= 373 (count (get-in s [:metrics :position :track]))))))

  (testing "Parsing activity detail with footpod and no GPS"
    (let [p (parse-file "test-resources/Forerunner50FirstExample.tcx")
          a (first (:activities p))]
      (is (= 1 (count (:activities p))))
      (is (= 5 (count (:segments a))))
      (is (= 1552 (count
                   (mapcat #(get-in % [:metrics :hr :track])
                           (:segments a)))))
      (is (= 1552 (count
                   (mapcat #(get-in % [:metrics :speed :track])
                           (:segments a)))))
      (is (= 1551 (count
                   (mapcat #(get-in % [:metrics :cadence :track])
                           (:segments a)))))))

  (testing "Parsing activity detail with power metrics"
    (let [p (parse-file "test-resources/PowerExample.tcx")
          a (first (:activities p))
          s (first (:segments a))]
      (is (= 1 (count (:activities p))))
      (is (= 1 (count (:segments a))))
      (is (= 1 (count (get-in s [:metrics :power :track]))))
      (is (= 2093 (get-in s [:metrics :power :max])))
      (is (= 1000 (get-in s [:metrics :power :avg]))))))

#+clj
(deftest activity-list-test
  (testing "Parsing activity list files, with individual summaries"
    (let [p (parse-file "test-resources/FitnessHistoryDirectory.tcx")]
      (is (= 7 (count (:activities p)))))))

#+clj
(deftest unsupported-elements-test
  (testing "Parsing files with unsupported elements"
    (let [p1 (parse-file "test-resources/FitnessCoursesDetail.tcx")
          p2 (parse-file "test-resources/FitnessCoursesDirectory.tcx")]
      
      (is (= 0 (count (:activities p1))))
      (is (= 0 (count (:activities p2)))))))


;; -----------------------------------------------------------------------------
;; Property-based tests
;; TODO
