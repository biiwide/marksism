(ns marksism.core
  (:require [marksism.pegdown :as peg]))

(defn parse
  "Parses a String of Markdown content.
  cons-node: Node Constructor (fn [node-type node-attrs node-children-or-string])
  markdown: Markdown content as a String

Two ready to use node constructors are provided: hiccup-sylte & data-style"
  [cons-node markdown]
  (peg/parse cons-node markdown))

(defn hiccup-style
  "A node constructor that returns Hiccup-like data:
  [:node-type (attrs-map)? child ...]"
  [type attrs content]
  (let [content-coll (if (coll? content)
                       content
                       (list content))]
    (cons type
      (if (empty? attrs)
        content-coll
        (cons attrs content-coll)))))

(defn data-style
  "A node constructor that returns a map in the style of clojure.data.xml and Enlive:
  {:tag type, :attrs attributes, :content content}"
  [type attrs content]
  (cond-> {:tag type :content content}
    (not-empty attrs)
    (assoc :attrs attrs)))