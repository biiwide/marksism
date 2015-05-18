(ns marksism.pegdown
  (:require [clojure.string :as str]
            [net.cgrand.enlive-html :as html]
            [marksism.node :refer :all])
  (:import  [java.io StringReader StringWriter]
            [org.pegdown.ast
             RootNode BulletListNode ListItemNode SuperNode TextNode RefLinkNode
             AnchorLinkNode AutoLinkNode BlockQuoteNode CodeNode  DefinitionNode
             DefinitionListNode DefinitionTermNode
             ExpImageNode ExpLinkNode HeaderNode HtmlBlockNode InlineHtmlNode
             MailLinkNode Node OrderedListNode ParaNode QuotedNode QuotedNode$Type
             SimpleNode SimpleNode$Type SpecialTextNode StrikeNode StrongEmphSuperNode
             TableNode TableBodyNode TableCaptionNode TableCellNode
             TableColumnNode TableColumnNode$Alignment TableHeaderNode TableRowNode
             TextNode
             VerbatimNode ReferenceNode]
           [org.pegdown PegDownProcessor Extensions]
           [org.w3c.tidy Tidy]))

(defn tidy
  "Tidy up HTML with Tidy"
  [untidy]
  (let [w (StringWriter.)]
    (doto (Tidy.)
      (.setTabsize 4)
      (.setPrintBodyOnly true)
      (.setShowWarnings false)
      (.setQuiet true)
      (.parse (StringReader. untidy) w))
    (-> (.toString w)
     (str/replace "\r\n" "\n")
     (str/replace "\r" "\n")
     (str/replace #"\n+" "\n"))))


(def options
  {:none Extensions/NONE
   :abbreviations Extensions/ABBREVIATIONS
   :anchorlinks Extensions/ANCHORLINKS
   :autolinks Extensions/AUTOLINKS
   :definitions Extensions/DEFINITIONS
   :fenced-code-blocks Extensions/FENCED_CODE_BLOCKS
   :hardwraps Extensions/HARDWRAPS
   :smarts Extensions/SMARTS
   :strikethrough Extensions/STRIKETHROUGH
   :tables Extensions/TABLES
   :quotes Extensions/QUOTES
   :wikilinks Extensions/WIKILINKS})

(defn parse-options [opts]
  (int (reduce bit-or
         (map options (cons :none opts)))))

(defn parse
  "Parse a string of mardown text.
The 'cons-node' function will be used to construct each
node.  It should expect three arguments: node type, node
attributes, & node content.  Node content will either be
a String, or a list of child nodes."
  [cons-node md-string & [opts]]
  (let [opts (or opts [:autolinks :fenced-code-blocks :hardwraps
                       :smarts :strikethrough :tables :quotes])]
    (->node
      (.parseMarkdown
        (PegDownProcessor. (parse-options opts))
        (char-array md-string))
      cons-node
      nil)))

(defn just-attributes
  "Node Constructor that only return the attributes map"
  [_ attrs _] attrs)

(defn just-content
  "Node Constructor that only returns the content"
  [_ _ content] content)

(letfn [(lift [n]
          (let [n-class (class n)]
            (cond (#{SuperNode RootNode} (class n))
                  (mapcat lift (.getChildren ^SuperNode n))

                  (#{TextNode SpecialTextNode} (class n))
                  (list (.getText ^TextNode n))

                  :else (list n))))]
  (defn lift-children [n]
    (combine-adjacent string? str
      (mapcat lift (.getChildren n)))))

(defn f-apply [f]
  (fn [node]
    (if (instance? Node node)
      (f node)
      node)))

(defn coll->node [node-coll cons-node state]
  (map (f-apply (curry->node cons-node state))
    node-coll))

(defn construct-children [n cons-node state]
  (coll->node (lift-children n) cons-node state))

(extend-protocol MarkdownNode RootNode
  (->node [^RootNode root cons-node state]
    (let [refs (index-by :id
                 (map (curry->node just-attributes state)
                      (.getReferences root)))
          abbrs (not-empty (map (curry->node cons-node state)
                                (.getAbbreviations root)))]
      (cons-node :markdown
        (cond-> nil
          refs
          (assoc :references refs)
          abbrs
          (assoc :abbreviations abbrs))
        (construct-children root cons-node
          (cond-> state
            refs (update-in [:references] merge refs)
            abbrs (update-in [:abbreviations] merge abbrs)))))))

(def ^:private always-nil (constantly nil))

(defn- super-node-handler [label ->attrs]
  (fn [md cons-node state]
    (cons-node label
      (->attrs md cons-node state)
      (construct-children md cons-node state))))

(defn super-node
  "Generic PegDown SuperNode parser."
  [class label ->attrs]
  (extend class MarkdownNode
    {:->node (super-node-handler label ->attrs)}))

(defn basic-super-node
  "Implement a basic SuperNode parser.  For Node types without attributes"
  [class label]
  (super-node class label always-nil))

(basic-super-node BlockQuoteNode :block-quote)
(basic-super-node BulletListNode :bullet-list)
(basic-super-node DefinitionListNode :definition-list)
(basic-super-node DefinitionNode :definition)
(basic-super-node DefinitionTermNode :definition-term)
(basic-super-node OrderedListNode :numbered-list)
(basic-super-node ListItemNode :list-item)
(basic-super-node ParaNode :paragraph)
(basic-super-node StrikeNode :strike)

(extend-protocol MarkdownNode
  ExpImageNode
  (->node [^ExpImageNode i cons-node state]
    (let [title (.title i)
          url (.url i)
          alt (first (construct-children i just-content state))]
      (cons-node :image
        (cond-> {:url (.url i)}
          (not (str/blank? alt))
          (assoc :alt alt)
          (not (str/blank? title))
          (assoc :title title)) nil))))

(super-node ExpLinkNode :link
  (fn [^ExpLinkNode l _ _]
    (let [title (.title l)]
      (cond-> {:url (.url l)}
        (not (str/blank? title))
        (assoc :title title)))))

(super-node HeaderNode :heading
  (fn [^HeaderNode h _ _]
    {:level (.getLevel h)}))

(extend-protocol MarkdownNode
  RefLinkNode
  (->node [r cons-node state]
    (let [children (construct-children r cons-node state)
          [[_ _ ref-key]] children
          ref (-> state :references (get ref-key))]
      (cons-node :link
        (assoc ref :ref ref-key)
        children)))

  ReferenceNode
  (->node [r cons-node state]
    (let [[id] (construct-children r just-content state)
          title (.getTitle r)]
      (cons-node :link-reference
        (cond-> {:url (.getUrl r)
                 :id id}
          (not (empty? title))
          (assoc :title title))
        (construct-children r cons-node state)))))

(super-node QuotedNode :quote
  (let [attrs {QuotedNode$Type/Single {:type :single
                                       :open "‘"
                                       :close "’"}
               QuotedNode$Type/Double {:type :double
                                       :open "“"
                                       :close "”"}
               QuotedNode$Type/DoubleAngle {:type :double-angle
                                            :open "«"
                                            :close "»"}}]
    (fn [^QuotedNode q _ _]
      (attrs (.getType q)))))

(super-node TableNode :table
  (fn [^TableNode t cons-node state]
    {:columns (map (curry->node cons-node state)
                (.getColumns t))}))

(basic-super-node TableHeaderNode :table-header)
(basic-super-node TableRowNode :table-row)

(super-node TableCellNode :table-cell
  (fn [^TableCellNode c _ _]
    (let [col-span (.getColSpan c)]
      (when (< 1 col-span)
        {:span-columns col-span}))))

(basic-super-node TableBodyNode :table-body)

(let [alignments {TableColumnNode$Alignment/None nil
                  TableColumnNode$Alignment/Left {:align :left}
                  TableColumnNode$Alignment/Right {:align :right}
                  TableColumnNode$Alignment/Center {:align :center}}]
  (super-node TableColumnNode :table-column
    (fn [^TableColumnNode c _ _]
      (alignments (.getAlignment c)))))

(letfn [(->coll [x]
          (if (coll? x)
            x (list x)))]
  (defn text-node [class label ->attrs & [xform-text]]
      (let [xformer (if (fn? xform-text)
                      (comp ->coll xform-text)
                      list)]
        (extend class MarkdownNode
          {:->node (fn [^TextNode txt-node cons-node state]
                     (cons-node label
                       (->attrs txt-node cons-node state)
                       (xformer (.getText txt-node))))}))))

(defn basic-text-node
  [class label & [xform-text]]
  (text-node class label always-nil xform-text))

(basic-text-node TextNode :text (comp first html/html-snippet))
(basic-text-node CodeNode :code-inline)
(basic-text-node HtmlBlockNode :html-block)
(basic-text-node InlineHtmlNode :inline-html)
(basic-text-node MailLinkNode :mailto)
(basic-text-node SpecialTextNode :text)

(text-node VerbatimNode :code-block
  (fn [^VerbatimNode v _ _]
    (when-some [type (.getType v)]
      {:type type})))

(text-node AnchorLinkNode :anchor
  (fn [^AnchorLinkNode a _ _]
    {:name (.getName a)}))


(extend StrongEmphSuperNode
  MarkdownNode
  {:->node (let [->attrs (fn [^StrongEmphSuperNode n _ _]
                           {:closed? (.isClosed n) :chars (.getChars n)})
                 strong-node (super-node-handler :strong ->attrs)
                 emph-node (super-node-handler :emphasis ->attrs)]
             (fn [^StrongEmphSuperNode n cons-node state]
               (if (.isStrong n)
                 (strong-node n cons-node state)
                 (emph-node n cons-node state))))})

(let [entities {SimpleNode$Type/Apostrophe {:type :apostrophe}
                SimpleNode$Type/Ellipsis   {:type :ellipsis}
                SimpleNode$Type/Emdash     {:type :emdash}
                SimpleNode$Type/Endash     {:type :endash}
                SimpleNode$Type/HRule      {:type :hrule}
                SimpleNode$Type/Linebreak  {:type :break}
                SimpleNode$Type/Nbsp       {:type :nbsp}}]
  (defn parse-simple-node [^SimpleNode s cons-node _]
    (cons-node :entity
      (entities (.getType s))
      nil)))

(extend SimpleNode
  MarkdownNode
  {:->node parse-simple-node})

;(extend-type HtmlBlockNode AstToClj
;  (to-clj [node cfg]
;    (html/html-snippet (tidy (.getText node)))))
;
;(extend-type InlineHtmlNode AstToClj
;  (to-clj [node cfg]
;    (html/html-snippet (tidy (.getText node)))))