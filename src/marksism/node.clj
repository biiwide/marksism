(ns marksism.node)

(defprotocol MarkdownNode
  (->node [md cons-node state] "Render a node as clojure data."))

(defn curry->node [cons-node state]
  (fn [md]
    (->node md cons-node state)))

(defn index-by [f coll]
  (reduce (fn [m v] (assoc m (f v) v))
    nil coll))

(defn combine-adjacent [pred combine coll]
  (not-empty
    (reduce (fn [result item]
              (let [prev-item (peek result)]
                (if (and (pred prev-item) (pred item))
                  (-> result pop
                    (conj (combine prev-item item)))
                  (conj result item))))
      [] coll)))
