
(defn get-file [path] 
  (with-open [rdr (java.io.BufferedReader. (java.io.FileReader. path))] 
    (doseq [line (line-seq rdr)] (println line))))

(get-file "C:\Users\clojure\Documents\some.txt")

;;(defn insert-word [word]
;;(if (nil? word)
 ;;nil
;; (and (add-to-trie (first word) (insert-word (rest word))))))

;;allows for str/some java string operation
(ns predictor 
  (:require [clojure.string :as str]))


(defrecord Node [letter children is-word?])

(def tree-two (Node. \b [] nil))

(def a-tree (Node. \a   [] nil))

(def tree-three (Node. \c   [] nil))

(def tree-top (Node. nil [] nil))

;;overly complicated get rid of anonymous function
(defn in-node? [letter children]
  ( (fn [l t] (= (:letter (first t)) l) ) letter children )) 

;;currently used version
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
 

;;works 
(defn adder [origin added]
  "adds a tree to original (origin) tree's children"
  (Node. (:letter origin) (cons added (:children origin)) (:is-word? origin)))

;;unused
(defn return-node
  "returns a node from a tree"
  [letters a-node]
 (get-node (first letters) (adder a-node (Node. (first letters) [] false))))

 (defn set-is-word
   "sets current node's is-word boolean true if it is the end of a word"
   [node is-last-letter]
   (assoc node :is-word? is-last-letter))

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

;;unused
(defn add-root
  [a-node]
  "adds tree to a nil root"
  (adder (Node. nil () nil) a-node))

;;;unused
(defn insert-word
  [letters current-node]
  (add-root (insert-letters letters current-node)))  

(defn insert-letters
  "insert a word into the tree"
  [letters current-node]
     (cond 
      (empty? letters) (set-is-word current-node true)
      (child-is-null? (first letters) current-node) (adder current-node (insert-letters (rest letters) (Node. (first letters) [] false) ))
      true  (adder (remove-node (first letters) current-node) (insert-letters (rest letters) (get-node (first letters) current-node)))))

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

(defn traverse-tree
  [prefix current-node]
  "returns a word that has 'prefix' as a prefix"
  (let [prefix-final-node (traverse-prefix prefix current-node)]
    (loop [this-node prefix-final-node list-of-words []]
      (cond
       (empty? (:children this-node)) (traverse-tree prefix )
       (list-full? list-of-words) list-of-words
       true (do
              )
;;;
       true (recur (first (:children this-node)) (concat list-of-words (check-children-for-valid-words (:children this-node) prefix list-of-words )))))))

;before this function call traverse-prefix to get to proper node then send in children
(defn traverse-tree
  [prefix children]
  (let [list-of-words (check-children-for-valid-words children prefix [])]
    (check-children-for-valid-words (:children (first children)) prefix list-of-words)
    (cond
     (empty? children) list-of-words
     (list-full? list-of-words) list-of-words
     true (do
            (traverse-tree prefix (rest children))
            (check-children-for-valid-words (:children (first children)) prefix list-of-words )))))


;;;current-works
(defn check-children-for-valid-words
  [children prefix list-of-words]
  "check children of current node to find valid words"
  (let [n 10];; get rid of this let
    (cond
      (list-full? list-of-words) list-of-words
      (empty? children) list-of-words
      (:is-word? (first children)) (check-children-for-valid-words (rest children) prefix (conj list-of-words (str prefix (:letter (first children)))))
      true (get-words-in-tree (rest children) prefix  list-of-words))))

(def treaty (adder tree-two tree-three))
(def entreat (adder a-tree treaty ))
(def treat (adder tree-top entreat) )
(defrecord Node [letter children is-word?])
(def tree-two (Node. \b [] nil))
(def a-tree (Node. \a   [] nil))
(def tree-three (Node. \c   [] nil))
(def tree-top (Node. nil [] nil))




;;;below are de-railed versions

(defn remove-node
  "removes a node (i.e. a letter and its children) from the tree"
  [letter a-node]
  (cond 
     (empty? a-node) nil
     (= letter (:letter (first a-node))) (rest a-node)
     true (concat (first a-node) (remove-node letter (rest a-node)))  ))

 

