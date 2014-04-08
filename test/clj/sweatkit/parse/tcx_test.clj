(ns sweatkit.parse.tcx-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [sweatkit.parse.tcx :refer :all]
            [clj-time.coerce :as tc]))

;; -----------------------------------------------------------------------------
;; Unit tests
;; Some example-based unit tests, to cover the basics

(deftest activity-detail-test
  (testing "Parsing activity detail with multiple metrics, including GPS"
    (let [r (io/file "test-resources/FitnessHistoryDetail.tcx")
          p (parse (.getPath r))
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
    (let [r (io/file "test-resources/Forerunner50FirstExample.tcx")
          p (parse (.getPath r))
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
    (let [r (io/file "test-resources/PowerExample.tcx")
          p (parse (.getPath r))
          a (first (:activities p))
          s (first (:segments a))]
      (is (= 1 (count (:activities p))))
      (is (= 1 (count (:segments a))))
      (is (= 1 (count (get-in s [:metrics :power :track]))))
      (is (= 2093 (get-in s [:metrics :power :max])))
      (is (= 1000 (get-in s [:metrics :power :avg]))))))

(deftest activity-list-test
  (testing "Parsing activity list files, with individual summaries"
    (let [r (io/file "test-resources/FitnessHistoryDirectory.tcx")
          p (parse (.getPath r))]
      (is (= 7 (count (:activities p)))))))

(deftest unsupported-elements-test
  (testing "Parsing files with unsupported elements"
    (let [r1 (io/file "test-resources/FitnessCoursesDetail.tcx")
          r2 (io/file "test-resources/FitnessCoursesDirectory.tcx")
          p1 (parse (.getPath r1))
          p2 (parse (.getPath r2))]
      
      (is (= 0 (count (:activities p1))))
      (is (= 0 (count (:activities p2)))))))


;; -----------------------------------------------------------------------------
;; Property-based tests
;; TODO
