(ns sweatkit.core-test
  (:require [sweatkit.core :as sk]
            #+clj  [clojure.test :as t :refer :all]  
            #+cljs [cemerick.cljs.test :as t]
            #+clj  [clj-time.coerce :as tc]
            #+cljs [cljs-time.coerce :as tc]
            #+clj  [clj-time.core :as time]
            #+cljs [cljs-time.core :as time])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))

;; -----------------------------------------------------------------------------
;; Unit tests

(defn- make-metric [m dtstart duration]
  (map #(hash-map :instant (tc/from-long (+ (tc/to-long dtstart)
                                            (min duration
                                                 (* 1000 (rand-int 5) (int %)))))
                  m (Math/abs (* 10 (Math/sin %))))
       (range 0 360 0.25)))

(def segment-1
  (let [dtstart (tc/from-string "2013-08-07T03:10:21Z")
        duration 4200000]
    {:dtstart dtstart
     :duration duration
     :annotations {:title "A segment"}
     :sport :cycling
     :active true
     :trigger :manual
     :metrics {:speed {:max 4.3
                       :track (make-metric :speed dtstart duration)}
               :distance {:total 1000}
               :cadence {:avg 50}}
     :other :stuff}))

(def segment-2
  (let [dtstart (tc/from-string "2013-08-07T04:20:14Z")
        duration 300000]
    {:dtstart dtstart
     :duration duration
     :sport :running
     :active false
     :trigger :manual
     :metrics {:speed {:track (make-metric :speed dtstart duration)}}
     :some "other"}))

(def segment-3
  (let [dtstart (tc/from-string "2013-08-07T04:25:14Z")
        duration 6000000] 
    {:dtstart dtstart
     :duration duration
     :sport :running
     :active true
     :metrics {:speed {:track (make-metric :speed dtstart duration)}}
     :trigger :speed}))

(def activity
  {:dtstart (tc/from-string "2013-08-07T03:10:21Z")
   :annotations {:title "An activity" :some :other}
   :segments [segment-1 segment-2 segment-3]})

(def sweat-db
  {:activities [activity]})

(deftest build-test
  (testing "Valid format should be built"
    (is (sk/build sweat-db)))

  (testing "Invalid format should return empty build"
    (let [db-1 (assoc-in sweat-db [:activities 0 :segments 1 :metrics] nil)]
      (is (not (sk/build db-1)))))

  (testing "Should return an mseq of Activities and each should be measured"
    (let [b (sk/build sweat-db)]
      (is (sk/measured? (:activities b)))
      (is (empty? (filter false? (map sk/measured? (:activities b))))))))

(deftest format-test
  (testing "Valid format should be true"
    (is (sk/valid-sweat? sweat-db)))

  (testing "Invalid format should be false"
    (let [db-1 (assoc-in sweat-db [:activities 0 :segments 0 :sport] nil)]
      (is (not (sk/valid-sweat? db-1))))))

(deftest mseq-test
  (testing "Measured coll with a repeating measured element"
    (let [b (sk/build sweat-db)
          ref-act (-> b :activities first)
          acts (sk/mseq [ref-act ref-act ref-act])]
      (testing "Becomes measured"
        (is (sk/measured? acts)))
      (testing "Duration is equal to to the reference activity (has the same dtstart/dtend)"
        (is (= (-> acts sk/interval sk/duration)
               (-> ref-act sk/interval sk/duration))))
      (testing "Total distance should be the sum of each measured element"
        (is (= (sk/mreduce acts :distance :total)
               (* (count (seq acts)) (sk/mreduce ref-act :distance :total)))))
      (testing "Max, Min and Avg values should be the same as the reference activity"
        (is (= (sk/mreduce acts :speed :max)
               (sk/mreduce ref-act :speed :max)))
        (is (= (sk/mreduce acts :speed :avg)
               (sk/mreduce ref-act :speed :avg)))))))

(deftest interpolate-test)

(deftest splits-test)

(deftest metrics-test)

(deftest predicates-test
  (testing "Should be true for a measured"
    (let [v (reify sk/IMeasured)]
      (is (sk/measured? v))))
  (testing "Should be false for any non-measured"
    (is (not (sk/measured? nil)))
    (is (not (sk/measured? [])))
    (is (not (sk/measured? {})))
    (is (not (sk/measured? "abc")))
    (is (not (sk/measured? 123))))
  (testing "Should be true for a point-val"
    (let [v (reify sk/IPointValue)]
      (is (sk/point-val? v))))
  (testing "Should be false for any non point-val"
    (is (not (sk/point-val? nil)))
    (is (not (sk/point-val? [])))
    (is (not (sk/point-val? {})))
    (is (not (sk/point-val? "abc")))
    (is (not (sk/point-val? 123)))))



