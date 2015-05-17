(ns marksism.core
  (:require [marksism.pegdown :as peg]))

(defn parse [to-node md-string]
  "Parses a String of Markdown content"
  (peg/parse to-node md-string))
