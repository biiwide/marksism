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

(def node-types
  #{:markdown :block-quote :bullet-list
    :definition-list :definition :definition-term
    :list-item :numbered-list
    :heading :paragraph :strike
    :image :link :link-reference :quote
    :table :table-body :table-cell :table-header
    :table-row :table-column
    :code-inline :html-block :inline-html
    :mailto :code-block :anchor :strong :emphasis
    :entity
    })

(defmacro deftransform [transform-name doc & node-transform-pairs]
  (assert (even? (count node-transform-pairs))
    "Every node type must have an associated transformation")
  (doseq [node (take-nth 2 node-transform-pairs)]
    (assert (node-types node)
      (format "Unrecognized node type '%s'" (pr-str node))))

  (let [xform (gensym "xform")]
    `(letfn [(~xform [~'return ~'node ~'attrs ~'content]
               (case ~'node
                 ~@node-transform-pairs
                 nil))]
       (defn ~transform-name ~doc
         [~'cons-node]
         (fn [~'node-type ~'node-attrs ~'node-content]
           (~xform ~'cons-node ~'node-type ~'node-attrs ~'node-content))))))