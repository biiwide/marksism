(ns marksism.core
  (:require [marksism.pegdown :as peg]))

(defn parse
  "Parses a String of Markdown content.
  cons-node: Node Constructor (fn [node-type node-attrs node-children-or-string])
  markdown: Markdown content as a String

Two ready to use node constructors are provided: `->hiccup` & `->data`"
  [cons-node markdown]
  (peg/parse cons-node markdown))

(defn ->hiccup
  "A node constructor that returns Hiccup-like data:
  [:node-type (attrs-map)? child ...]"
  [type attrs content]
  (let [content-coll (cond (coll? content) content
                           (nil? content) nil
                           :else (list content))]
    (cons type
      (if (empty? attrs)
        content-coll
        (cons attrs content-coll)))))

(defn ->xml
  "A node constructor that returns a map in the style of clojure.data.xml and Enlive:
  {:tag type, :attrs attributes, :content content}"
  [type attrs content]
  (cond-> {:tag type :content content}
    (not-empty attrs)
    (assoc :attrs attrs)))


(letfn [(f-apply [f]
          (fn [node]
            (if (fn? node)
              (node f)
              node)))]
  (defn ->free
    "A node constructor that returns a free applicative functor of the parse tree.
In other words, it returns a function that accepts a node constructor function.
This allows you to parse a document once, and re-interpret the same parse tree
by applying different node constructors"
    [type attrs content]
    (fn [cons-node]
      (cons-node type attrs
        (map (f-apply cons-node) content)))))