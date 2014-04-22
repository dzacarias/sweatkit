(ns sweatkit.formats.impl.xml
  "Helper implementation namespace for XML handling"
  (:require        [clojure.zip :as zip]
            #+cljs [goog.dom.xml :as xml]
            #+clj  [clojure.xml :as xml]
            #+clj  [clojure.java.io :as io]
            #+clj  [clj-time.format :as time]
            #+cljs [cljs-time.format :as time]))

;; ==============================================================================
;; XML Helpers

#+cljs
(defrecord XMLElement [tag attrs content])

#+cljs
(defn- xml-node
  ([node] (xml-node node [(.-textContent node)]))
  ([node inner]
     (when-not (nil? node)
       (->XMLElement (keyword (.-tagName node))
                     (apply merge (map #(let [i (.item (.-attributes node) %)]
                                          (hash-map (keyword (.-name i))
                                                    (.-value i)))
                                       (range (.-length (.-attributes node)))))
                     inner)))) 

#+clj
(defn parse
  "Proxy for clojure.xml/parse"
  [s] (xml/parse s))

#+cljs
(defn parse
  "Takes a String, representing an XML document and returns a
   clojure.xml-compatible data structure"
  [doc]
  (let [doc (xml/loadXml doc)
        root (.-documentElement doc)]
    (loop [parent root
           node (.-firstElementChild parent)
           nested [[]]]
      (if (nil? node)
        (if (= parent root)
           ; at the end, return root node with nested vals
          (xml-node root (last nested))
           ; no more siblings, go up and to the right
          (let [others (pop nested)
                elders (pop others)
                siblings (last others)
                children (last nested)]
            (recur (.-parentNode parent)
                   (.-nextElementSibling parent)
                   (conj elders (conj siblings (xml-node parent children))))))
        (if (pos? (.-childElementCount node))
           ; has children, go down
          (recur node
                 (.-firstElementChild node)
                 (conj nested []))
           ; no children, go right to next sibling
          (recur parent 
                 (.-nextElementSibling node) 
                 (conj (pop nested)
                       (conj (last nested)
                             (xml-node node)))))))))

(defn parse-double [txt]
  #+clj
  (Double/parseDouble txt)
  #+cljs
  (js/parseFloat txt))

(defn parse-int [txt]
  #+clj
  (Integer/parseInt txt)
  #+cljs
  (js/parseInt txt))

;; Adapted from clojure.data.zip
(defn- right-locs
  "Returns a lazy sequence of locations to the right of loc, starting with loc."
  [loc]
  (lazy-seq (when loc (cons loc (right-locs (zip/right loc))))))

(defn xml->
  "Emulates clojure.data.zip's xml->, but only takes a sequence of keywords
   to use as filters to zip down from a top loc"
  [loc & preds]
  (loop [locs (right-locs (zip/down loc))
         tags preds]
    (let [v (filter #(= (:tag (zip/node %)) (first tags)) locs)]
      (if (empty? (next tags))
          v
          (recur (mapcat #(when (zip/branch? %)
                            (right-locs (zip/down %)))
                         v)
                 (next tags))))))

(defn xml1->
  "Returns the first result from xml->"
  [loc & preds]
  (first (apply xml-> loc preds)))

(defn xml1->text [loc & preds] 
  (some-> (apply xml1-> loc preds) zip/node :content first))

(defn xml1->double [loc & preds]
  (some-> (apply xml1->text loc preds) parse-double))

(defn xml1->int [loc & preds]
  (some-> (apply xml1->text loc preds) parse-int))

(defn xml1->inst [loc & preds]
  (some-> (apply xml1->text loc preds) time/parse))

(defn attr [loc name]
  (when (zip/branch? loc) (-> loc zip/node :attrs name)))
