(ns marksism.html
  (:require [marksism.core :as m]
            [marksism.node :refer :all]))

(deftransform ->html
  "Translate Markdown to HTML"

  :markdown
  (return :html nil
    [(return :body nil content)])

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

  :quote
  (return :q {:class (str "quote-" (:type attrs))} content)

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

  :code-inline
  (return :code {:class "code-inline"} content)

  :code-block
  (return :blockquote {:class "code-block"} content)
  
  )

  