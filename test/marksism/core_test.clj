(ns marksism.core-test
  (:require [clojure.test :refer :all]
            [marksism.core :refer :all]))


(deftest test->hiccup
  (are [args expected]
    (= expected (apply ->hiccup args))

    [:type nil "text"]
    [:type "text"]

    [:type nil nil]
    [:type]

    [:type nil (list)]
    [:type]

    [:type {} "text"]
    [:type "text"]

    [:type {:foo :bar} "text"]
    [:type {:foo :bar} "text"]

    [:parent nil [[:child "foo"] [:child "bar"]]]
    [:parent [:child "foo"] [:child "bar"]]

    [:parent {:key :val} [[:child "one"] "two" [:child "three"]]]
    [:parent {:key :val} [:child "one"] "two" [:child "three"]]
    ))


(deftest test->xml
  (are [args expected]
    (= expected (apply ->xml args))
 
    [:elem nil "text"]
    {:tag :elem :content "text"}
 
    [:elem {:foo :bar} "text"]
    {:tag :elem :attrs {:foo :bar} :content "text"}))


(deftest parse-fragments
  (are [fragment expected]
    (= expected (parse ->hiccup fragment))

    "Para One\n\nPara Two\n"
    [:markdown
     [:paragraph "Para One"]
     [:paragraph "Para Two"]]

    "* 1\n* 2"
    [:markdown
     [:bullet-list
      [:list-item "1"]
      [:list-item "2"]]]

    "+ 1\n+ 2"
    [:markdown
     [:bullet-list
      [:list-item "1"]
      [:list-item "2"]]]

    "- 1\n- 2"
    [:markdown
     [:bullet-list
      [:list-item "1"]
      [:list-item "2"]]]

    "1. Eins\n2. Zwei"
    [:markdown
     [:numbered-list
      [:list-item "Eins"]
      [:list-item "Zwei"]]]

    "3. Eins\n1. Zwei\n2. Drei"
    [:markdown
     [:numbered-list
      [:list-item "Eins"]
      [:list-item "Zwei"]
      [:list-item "Drei"]]]

    "![Alt text](/path/to/img.jpg \"Optional title\")"
    [:markdown
     [:paragraph
      [:image {:title "Optional title"
               :url "/path/to/img.jpg"
               :alt "Alt text"}]]]

    "*Foo*"
    [:markdown
     [:paragraph
      [:emphasis {:closed? true :chars "*"} "Foo"]]]

    "**Bar**"
    [:markdown
     [:paragraph
      [:strong {:closed? true :chars "**"} "Bar"]]]

    "* * *"
    [:markdown
     [:entity {:type :hrule}]]

    "> Line One\n> Line Two\n"
    [:markdown
     [:block-quote
      [:paragraph
       "Line One"
       [:entity {:type :break}]
       "Line Two"]]]

    "```some-language\nCode\nBlock\nHere\n```"
    [:markdown
     [:code-block {:type "some-language"} "Code\nBlock\nHere\n"]]

    "# H One\n## H Two"
    [:markdown
     [:heading {:level 1}
      [:anchor {:name "h-one"} "H One"]]
     [:heading {:level 2}
      [:anchor {:name "h-two"} "H Two"]]]

    "First Header | Second Header
-------------- | --------------
Content Cell 1 | Content Cell 2
Content Cell 3 | Content Cell 4"
    [:markdown
     [:table {:columns [[:table-column]
                        [:table-column]]}
      [:table-header
       [:table-row
        [:table-cell "First Header "]
        [:table-cell "Second Header"]]]
      [:table-body
       [:table-row
        [:table-cell "Content Cell 1 "]
        [:table-cell "Content Cell 2"]]
       [:table-row
        [:table-cell "Content Cell 3 "]
        [:table-cell "Content Cell 4"]]]]]

    "# Heading One\n* Item _One_\n* Item __Two__\n"
    [:markdown
     [:heading {:level 1}
      [:anchor {:name "heading-one"} "Heading One"]]
     [:bullet-list
      [:list-item "Item " [:emphasis {:closed? true :chars "_"} "One"]]
      [:list-item "Item " [:strong {:closed? true :chars "__"} "Two"]]]]
    ))


(deftest test-parse-with->free
  (let [content "# Heading One\n* Item _One_\n* Item __Two__\n"
        free (parse ->free content)]
    (are [constructor]
      (= (parse constructor content)
         (free constructor))

      ->hiccup
      ->xml
      vector
      list*
    )))
