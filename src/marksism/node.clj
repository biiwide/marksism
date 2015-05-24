(ns marksism.node)

(defprotocol MarkdownNode
  (->node [md cons-node state] "Render a node as clojure data."))

(defprotocol Liftable
  (contents [x] "Returns a sequence of the nested content"))

(defrecord Elevator [c]
  Liftable
  (contents [_] c))

(defn liftable? [x]
  (satisfies? Liftable x))

(defn elevate [x]
  (->Elevator x))

(defn flatten-nodes [coll]
  (remove liftable?
    (rest (tree-seq liftable? contents (elevate coll)))))

(defn flatten-content [cons-node]
  (fn [node attrs content]
    (cons-node node attrs (flatten-nodes content))))
  

(defn curry->node [cons-node state]
  (fn [md]
    (->node md cons-node state)))

(defn index-by [f coll]
  (reduce (fn [m v] (assoc m (f v) v))
    nil coll))

(defn combine-adjacent
  "Given a colletcion 'coll' combine adjacent elements which
satisfy the 'pred' function using the 'combine' function"
  [pred combine coll]
  (if (empty? coll)
    coll
    (not-empty
      (reduce (fn [result item]
                (let [prev-item (peek result)]
                  (if (and (pred prev-item) (pred item))
                    (-> result pop
                      (conj (combine prev-item item)))
                    (conj result item))))
        [(first coll)] (rest coll)))))

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

(defmacro deftranslator
  "A macro for constructing pass-through content translations.
The macro provides four bindings:
 return   :- A function of three arguments for returning a node.
             (return :node-type {:attr \"map\"} contents-seq)
 node     :- A keyword representing the type of node
 attrs    :- A map of node attributes
 contents :- A sequence of child nodes or text

Additionally, to translate a single node into a collection of
nodes, wrap the collection with the elevate function which will
allow the new collection to be flattened into the contents of
the parent node"
  [translator-name doc & node-translation-pairs]
  (let [xform (gensym "xform")
        node-handlers (take-nth 2 node-translation-pairs)
        missing-handlers (apply disj node-types node-handlers)]

    (assert (even? (count node-translation-pairs))
      "Every node type must have an associated translation")

    (doseq [node node-handlers]
      (assert (node-types node)
        (format "Unrecognized node type '%s'" (pr-str node))))

    (when-not (empty? missing-handlers)
      (binding [*out* *err*]
        (printf "Transform %s/%s:  Missing handlers for the following node types: %s\n"
          *ns* translator-name (pr-str missing-handlers))))

    `(letfn [(~xform [~'return ~'node ~'attrs ~'content]
               (case ~'node
                 ~@node-translation-pairs
                 nil))]
       (defn ~translator-name ~doc
         [~'cons-node]
         (fn [~'node-type ~'node-attrs ~'node-content]
           (~xform (flatten-content ~'cons-node)
             ~'node-type ~'node-attrs ~'node-content))))))