(ns marksism.node)

(defprotocol MarkdownNode
  (->node [md to-node state] "Render a node as clojure data."))

(defn curry->node [to-node state]
  (fn [md]
    (->node md to-node state)))