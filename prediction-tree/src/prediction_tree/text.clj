(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



;;allows for str/some java string operation
(ns predictor 
  (:require [clojure.string :as str]))

(defrecord Node [letter children is-word?])


(defn in-node? 
  [letter children]
  "checks to see if a node for a letter is contained in the children of the current node"
  (= (:letter (first children)) letter))

 (defn get-node
     "check if children in a tree's vector contain character, return child if yes"
     [letter tree]
     (loop [aletter letter childs (:children tree)]
       (cond
        (empty? childs)
        nil
        (in-node? aletter childs)
        (first childs)   
        true
        (recur aletter (rest childs)))))

(defn has-word?
  [word tree]
  "returns true if trie contains word"
  (loop [letters word current-node tree]
    (let [child-of-current-node (get-node (first letters) current-node)]
        (cond
         (empty? letters) (:is-word? current-node)
         (nil? child-of-current-node) false
         true (recur (rest letters) child-of-current-node)))))

(defn traverse-prefix
  [prefix current-node]
  "traverses a trie to the end of the prefix"
  (loop [pre prefix a-node current-node]
    (cond 
     (empty? pre) a-node
    (nil? a-node) nil
    true (recur (rest pre) (get-node (first pre) a-node)))))

(defn list-full?
  "checks if a list has the required number of elements in list"
  [list-of-words]
  (let [maximum-number-of-words 10]
    (>= (count list-of-words) 10)))

(defn trav-tree
  [node-list prefix list-of-words]
  (cond
   (empty? node-list) list-of-words
   true (do
          (concat (process-first (first node-list) prefix list-of-words) (trav-tree (rest node-list) prefix list-of-words)))))

(defn process-first
  [current-node prefix list-of-words]
  (cond
   (nil? current-node) list-of-words
   (list-full? list-of-words) list-of-words
   (not (empty? (:children current-node))) 
   (do
          (process-first (first (:children current-node)) (str prefix (:letter (first (:children current-node)))) (concat list-of-words (check-children-for-valid-words (:children current-node) prefix (trav-tree (rest (:children current-node)) prefix list-of-words)))) 
          )
   true list-of-words))

(defn check-children-for-valid-words
  [children prefix list-of-words]
  "check children of current node to find valid words"
  (let [n 10];; get rid of this let
    (cond
      (list-full? list-of-words) list-of-words
      (empty? children) list-of-words
      (:is-word? (first children)) (check-children-for-valid-words (rest children) prefix (conj list-of-words (str prefix (:letter (first children)))))
      true (check-children-for-valid-words (rest children) prefix  list-of-words))))

 

