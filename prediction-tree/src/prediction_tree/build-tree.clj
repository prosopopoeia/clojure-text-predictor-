
(ns predictor 
  (:require [clojure.string :as str]))

(get-file "C:\Users\clojure\Documents\some.txt")

(defrecord Node [letter children is-word?])

(defn get-file [path] 
  (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. path))] 
    (doseq [line (line-seq rdr)] (println line))))


(defn adder [origin added]
  "adds a tree to original (origin) tree's children"
  (Node. (:letter origin) (cons added (:children origin)) (:is-word? origin)))
  
(defn set-is-word
   "sets current node's is-word boolean true if it is the end of a word"
   [node is-last-letter]
   (assoc node :is-word? is-last-letter))
   
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
		
(defn child-is-null?
  "checks if a node's child is null "
  [letter a-node]
  (nil? (get-node letter a-node)))

(defn remove-child
  "removes a child-node (i.e. a letter and its children) from the tree"
  [letter a-node]
  (cond
   ;;;this cannot happen in context of the trie
     (empty? a-node) nil
     (= letter (:letter (first a-node))) (rest a-node)
     true  (concat #{(first a-node)}  (remove-child letter (rest a-node)))))

(defn remove-node
  [letter a-node]
  "removes a node from a tree"
  (Node. (:letter a-node) (remove-child letter (:children a-node)) nil))

(defn insert-letters
  "insert a word into the tree"
  [letters current-node]
     (cond 
      (empty? letters) (set-is-word current-node true)
      (child-is-null? (first letters) current-node) (adder current-node (insert-letters (rest letters) (Node. (first letters) [] false) ))
      true  (adder (remove-node (first letters) current-node) (insert-letters (rest letters) (get-node (first letters) current-node)))))

(defn in-node? 
  [letter children]
  "checks to see if a node for a letter is contained in the children of the current node"
  (= (:letter (first children)) letter))
