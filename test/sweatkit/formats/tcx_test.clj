(ns sweatkit.formats.tcx-test
  (:require [clojure.test :as t :refer :all]
            [clj-time.coerce :as tc]
            [sweatkit.formats.tcx :as tcx :refer (parse emit)]
            [sweatkit.core :as sk]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]))

;; -----------------------------------------------------------------------------
;; Unit tests

(deftest activity-detail-test
  (testing "Activity detail with multiple metrics, including GPS"
    (letfn [(test-fn [p]
              (let [a (first (:activities p))
                    s (first (:segments a))]
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
                (is (= 373 (count (get-in s [:metrics :position :track]))))))]
      (let [po (parse "test-resources/tcx/FitnessHistoryDetail.tcx")]
        (testing "Parsing"
          (test-fn po))
        (testing "Parsing/Emitting round-trip does not affect values"
          (let [f "/tmp/sweatkit.xml"]
            (with-open [out (-> (io/file f) io/output-stream io/writer)]
              (xml/emit (emit (sk/db po)) out))
            (test-fn (parse f)))))))

  (testing "Activity detail with footpod and no GPS"
    (letfn [(test-fn [p]
              (let [a (first (:activities p))]
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
                                     (:segments a)))))))]
      (let [po (parse "test-resources/tcx/Forerunner50FirstExample.tcx")]
        (testing "Parsing"
          (test-fn po))
        (testing "Parsing/Emitting round-trip does not affect values"
          (let [f "/tmp/sweatkit.xml"]
            (with-open [out (-> (io/file f) io/output-stream io/writer)]
              (xml/emit (emit (sk/db po)) out))
            (test-fn (parse f)))))))

  
  (testing "Activity detail with power metrics"
    (letfn [(test-fn [p]
              (let [a (first (:activities p))
                    s (first (:segments a))]
                (is (= 1 (count (:activities p))))
                (is (= 1 (count (:segments a))))
                (is (= 1 (count (get-in s [:metrics :power :track]))))
                (is (= 2093 (get-in s [:metrics :power :max])))
                (is (= 1000 (get-in s [:metrics :power :avg])))))]
      (let [po (parse "test-resources/tcx/PowerExample.tcx")]
        (testing "Parsing"
          (test-fn po))
        (testing "Parsing/Emitting round-trip does not affect values"
          (let [f "/tmp/sweatkit.xml"]
            (with-open [out (-> (io/file f) io/output-stream io/writer)]
              (xml/emit (emit (sk/db po)) out))
            (test-fn (parse f))))))))

(deftest multisport-session-test
  (testing "Multisport session with no transitions"
    (letfn [(test-fn
              [p]
              (let [a (first (:activities p))
                    s1 (nth (:segments a) 0)]
                (is (= 1 (count (:activities p))))
                (is (= 14 (count (:segments a))))
                (is (= 6.8734031 (get-in s1 [:metrics :speed :max])))
                (is (= 76 (count (get-in (nth (:segments a) 0)
                                         [:metrics :altitude :track]))))
                (is (= 70 (count (get-in (nth (:segments a) 1)
                                         [:metrics :altitude :track]))))
                (is (= 75 (count (get-in (nth (:segments a) 2)
                                         [:metrics :altitude :track]))))
                (is (= 74 (count (get-in (nth (:segments a) 3)
                                         [:metrics :altitude :track]))))
                (is (= 74 (count (get-in (nth (:segments a) 4)
                                         [:metrics :altitude :track]))))
                (is (= 69 (count (get-in (nth (:segments a) 5)
                                         [:metrics :altitude :track]))))
                (is (= 72 (count (get-in (nth (:segments a) 13)
                                         [:metrics :altitude :track]))))))]
      (let [po (parse "test-resources/tcx/Multisport.tcx")]
        (testing "Parsing"
          (test-fn po))
        (testing "Parsing/Emitting round-trip does not affect values"
          (let [f "/tmp/sweatkit.xml"]
            (with-open [out (-> (io/file f) io/output-stream io/writer)]
              (xml/emit (emit (sk/db po)) out))
            (test-fn (parse f))))))))

(deftest activity-list-test
  (testing "Activity list files, with individual summaries"
    (let [p (parse "test-resources/tcx/FitnessHistoryDirectory.tcx")
          dbp (sk/db p)
          f "/tmp/sweatkit.xml"]
      (testing "Parsing"
        (is (= 7 (count (:activities p)))))
      (testing "Parsing/Emitting round-trip does not affect values"
        (with-open [out (-> (io/file f) io/output-stream io/writer)]
          (xml/emit (emit dbp) out))
        (is (= 7 (count (:activities (parse f)))))))))

(deftest unsupported-elements-test
  (testing "Parsing files with unsupported elements"
    (let [p1 (parse "test-resources/tcx/FitnessCoursesDetail.tcx")
          p2 (parse "test-resources/tcx/FitnessCoursesDirectory.tcx")]
      
      (is (= 0 (count (:activities p1))))
      (is (= 0 (count (:activities p2)))))))
