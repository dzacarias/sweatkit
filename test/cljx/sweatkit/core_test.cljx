(ns sweatkit.core-test
  (:require [sweatkit.core :as sk]
            #+clj [clojure.test :as t
                   :refer (is deftest with-test run-tests testing)]
            #+cljs [cemerick.cljs.test :as t]
            [cemerick.double-check :as sc]
            [cemerick.double-check.generators :as gen]
            [cemerick.double-check.properties :as prop :include-macros true]
            [cemerick.double-check.clojure-test :refer (defspec) :include-macros true]
            #+clj  [clj-time.coerce :as tc]
            #+cljs [cljs-time.coerce :as tc]
      )
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]))

(defspec prop-true-for-any-measured
  10
  (prop/for-all [v (gen/fmap
                    (fn [& x]
                      (reify sk/IMeasured
                        (interval [this])
                        (metrics [this])
                        (tracked? [this metric])
                        (track [this metric])
                        (mreduce [this metric rfn])))
                    gen/any)]
                (true? (sk/measured? v))))

(defspec prop-false-for-any-non-measured
  10
  (prop/for-all [v gen/any]
                (false? (sk/measured? v))))

(defspec prop-true-for-any-point-val
  10
  (prop/for-all [v (gen/fmap
                    (fn [& x]
                      (reify sk/IPointValue
                        (inst [this])
                        (value [this])
                        (metric [this])))
                    gen/any)]
                (true? (sk/point-val? v))))

(defspec prop-false-for-any-non-point-val
  10
  (prop/for-all [v gen/any]
                (false? (sk/point-val? v))))

;; (def datetime-gen
;;   (gen/fmap tc/from-long gen/pos-int))

;; (def interval-gen
;;   (gen/fmap
;;    (fn [x]
;;      reify sk/IInterval
;;      )
;;    gen/any))
