(ns marksism.html
  (:require [marksism.core :as m]
            [marksism.node :refer :all]
            [net.cgrand.enlive-html :as enlive]
            [net.cgrand.jsoup :as jsoup])
  (:import  (java.io ByteArrayInputStream)))

(defn- string->stream [str]
  (when (some? str)
    (ByteArrayInputStream. (.getBytes str))))

(defn parse-html-str [s]
  (-> s
    string->stream
    jsoup/parser
    (enlive/select [:body])
    (->> (mapcat :content))))

(defn cons-enlive-node [cons-node]
  (fn enlive->node [enlive-node]
    (cond (string? enlive-node)
          enlive-node

          (map? enlive-node)
          (cons-node
            (:tag enlive-node)
            (:attrs enlive-node)
            (map enlive->node
              (:content enlive-node)))

          :else (throw (Exception.
                         (str "Don't know how to handle Enlive node: "
                           (pr-str enlive-node)))))))

(deftranslator ->html
  "Translate Markdown to HTML"

  :markdown
  (return :html nil
    [(return :body nil content)])

  :anchor
  (return :a {:id (:name attrs)} content)

  :paragraph
  (return :p {:class "paragraph"} content)

  :bullet-list
  (return :ul {:class "bullet-list"} content)

  :numbered-list
  (return :ol {:class "numbered-list"} content)

  :list-item
  (return :li {:class "list-item"} content)

  :block-quote
  (return :blockquote {:class "block-quote"} content)

  :definition-list
  (return :dl nil content)

  :definition-term
  (return :dt nil content)

  :definition
  (return :dd nil content)

  :heading
  (return (case (:level attrs)
            1 :h1 2 :h2 3 :h3
            4 :h4 5 :h5 6 :h6 :h1)
          {:class "heading"} content)

  :emphasis
  (return :em {:class "emphasis"} content)

  :strong
  (return :strong {:class "strong"} content)

  :strike
  (return :strike {:class "strike"} content)

  :image
  (return :img (let [url (:url attrs)]
                 (-> attrs
                   (dissoc :url)
                   (assoc :src url)))
          content)

  :link
  (return :a {:class "link"
              :href (:url attrs)} content)

  :link-reference
  (return :span {:class "link-reference"} content)

  :inline-html
  (elevate content)

  :html-block
  (elevate (map (cons-enlive-node return)
             (mapcat parse-html-str content)))

  :quote
  (return :q {:class (:type attrs)} content)

  :table
  (return :table nil content)

  :table-body
  (return :tbody nil content)

  :table-header
  (return :thead nil content)

  :table-row
  (return :tr nil content)

  :table-cell
  (return :td (when-some [colspan (:span-columns attrs)]
                {:colspan colspan}) content)

  :table-column
  (elevate nil)

  :code-inline
  (return :code {:class "code-inline"} content)

  :code-block
  (return :pre {:class "code-block"}
    [(return :code {:class "code-block"} content)])

  :entity
  (case (:type attrs)
    :apostrophe "&apos;"
    :ellipsis "&hellip;"
    :emdash "&mdash;"
    :endash "&ndash;"
    :hrule  (return :hr nil nil)
    :break  (return :br nil nil)
    :nbsp   "&nbsp;")

  :mailto
  (return :a {:href (first content)} content)
  )

  