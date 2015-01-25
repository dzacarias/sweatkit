(ns sweatkit.formats.tcx-test
  (:require [clojure.test :as t :refer :all]
            [clj-time.coerce :as tc]
            [sweatkit.formats.tcx :as tcx :refer (parse emit)]
            [sweatkit.test-util :as util]
            [sweatkit.core :as sk]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

;; -----------------------------------------------------------------------------
;; Unit tests

(deftest activity-detail-test
  (testing "Activity detail with multiple metrics, including GPS"
    (let [p (-> (util/read-file "test-resources/tcx/FitnessHistoryDetail.tcx") parse)
          a (first (:activities p))
          s (first (:segments a))
          dbp (sk/db p)
          f "/tmp/sweatkit.xml"]
      (testing "Parsing"
        (is (= 1 (count (:activities p))))
        (is (.equals (tc/from-string "2007-08-07T02:42:41Z") (:dtstart a)))
        (is (= (sort (keys (:metrics s)))
               (sort [:distance :speed :calories :position :altitude])))
        (is (= 285 (get-in s [:metrics :calories :total])))
        (is (= 8348.5039063 (get-in s [:metrics :distance :total])))
        (is (= 18.6828499 (get-in s [:metrics :speed :max])))
        (is (:active s))
        (is (= :manual (:trigger s)))
        (is (= 373 (count (get-in s [:metrics :distance :track]))))
        (is (= 373 (count (get-in s [:metrics :altitude :track]))))
        (is (= 373 (count (get-in s [:metrics :position :track])))))
      (testing "Parsing/Emitting round-trip does not affect values"
        (with-open [out (-> (io/file f) io/output-stream io/writer)]
          (xml/emit (emit dbp) out))
        (is (= dbp) (sk/db (parse f))))))

  (testing "Activity detail with footpod and no GPS"
    (let [p (-> (util/read-file "test-resources/tcx/Forerunner50FirstExample.tcx") parse)
          a (first (:activities p))
          dbp (sk/db p)
          f "/tmp/sweatkit.xml"]
      (testing "Parsing"
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
                             (:segments a))))))
      (testing "Parsing/Emitting round-trip does not affect values"
        (with-open [out (-> (io/file f) io/output-stream io/writer)]
          (xml/emit (emit dbp) out))
        (is (= dbp) (sk/db (parse f))))))

  (testing "Activity detail with power metrics"
    (let [p (-> (util/read-file "test-resources/tcx/PowerExample.tcx") parse)
          a (first (:activities p))
          s (first (:segments a))
          dbp (sk/db p)
          f "/tmp/sweatkit.xml"]
      (testing "Parsing"
        (is (= 1 (count (:activities p))))
        (is (= 1 (count (:segments a))))
        (is (= 1 (count (get-in s [:metrics :power :track]))))
        (is (= 2093 (get-in s [:metrics :power :max])))
        (is (= 1000 (get-in s [:metrics :power :avg]))))

      (testing "Parsing/Emitting round-trip does not affect values"
        (with-open [out (-> (io/file f) io/output-stream io/writer)]
          (xml/emit (emit dbp) out))
        (is (= dbp) (sk/db (parse f)))))))

(deftest activity-list-test
  (testing "Activity list files, with individual summaries"
    (let [p (-> (util/read-file "test-resources/tcx/FitnessHistoryDirectory.tcx") parse)
          dbp (sk/db p)
          f "/tmp/sweatkit.xml"]
      (testing "Parsing"
        (is (= 7 (count (:activities p)))))
      (testing "Parsing/Emitting round-trip does not affect values"
        (with-open [out (-> (io/file f) io/output-stream io/writer)]
          (xml/emit (emit dbp) out))
        (is (= dbp) (sk/db (parse f)))))))

(deftest unsupported-elements-test
  (testing "Parsing files with unsupported elements"
    (let [p1 (-> (util/read-file "test-resources/tcx/FitnessCoursesDetail.tcx") parse)
          p2 (-> (util/read-file "test-resources/tcx/FitnessCoursesDirectory.tcx") parse)]
      
      (is (= 0 (count (:activities p1))))
      (is (= 0 (count (:activities p2)))))))
