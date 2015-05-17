(ns marksism.core-test
  (:require [clojure.test :refer :all]
            [marksism.core :refer :all]))


(defn ->node [type attrs children]
  (if (string? children)
    (vector type attrs children)
    (apply vector type attrs children)))


(deftest parse-fragments
  (are [fragment expected]
    (= expected (parse ->node fragment))

    "Para One\n\nPara Two\n"
    [:markdown nil
     [:paragraph nil
      [:text nil "Para One"]]
     [:paragraph nil
      [:text nil "Para Two"]]]

    "* 1\n* 2"
    [:markdown nil
     [:bullet-list nil
      [:list-item nil [:text nil "1"]]
      [:list-item nil [:text nil "2"]]]]

    "+ 1\n+ 2"
    [:markdown nil
     [:bullet-list nil
      [:list-item nil [:text nil "1"]]
      [:list-item nil [:text nil "2"]]]]

    "- 1\n- 2"
    [:markdown nil
     [:bullet-list nil
      [:list-item nil [:text nil "1"]]
      [:list-item nil [:text nil "2"]]]]

    "1. Eins\n2. Zwei"
    [:markdown nil
     [:numbered-list nil
      [:list-item nil [:text nil "Eins"]]
      [:list-item nil [:text nil "Zwei"]]]]

    "3. Eins\n1. Zwei\n2. Drei"
    [:markdown nil
     [:numbered-list nil
      [:list-item nil [:text nil "Eins"]]
      [:list-item nil [:text nil "Zwei"]]
      [:list-item nil [:text nil "Drei"]]]]

    "![Alt text](/path/to/img.jpg \"Optional title\")"
    [:markdown nil
     [:paragraph nil
      [:image {:title "Optional title"
               :url "/path/to/img.jpg"
               :alt "Alt text"}]]]

    "*Foo*"
    [:markdown nil
     [:paragraph nil
      [:emphasis {:closed? true :chars "*"}
       [:text nil "Foo"]]]]

    "**Bar**"
    [:markdown nil
     [:paragraph nil
      [:strong {:closed? true :chars "**"}
       [:text nil "Bar"]]]]

    "* * *"
    [:markdown nil
     [:entity {:type :hrule}]]

    "> Line One\n> Line Two\n"
    [:markdown nil
     [:block-quote nil
      [:paragraph nil
       [:text nil "Line One"]
       [:entity {:type :break}]
       [:text nil "Line Two"]]]]

    "```some-language\nCode\nBlock\nHere\n```"
    [:markdown nil
     [:code-block {:type "some-language"} "Code\nBlock\nHere\n"]]

    "First Header | Second Header
-------------- | --------------
Content Cell 1 | Content Cell 2
Content Cell 3 | Content Cell 4"
    [:markdown nil
     [:table {:columnts [[:table-column nil]
                         [:table-column nil]]}
      [:table-header nil
       [:table-row nil
        [:table-cell nil [:text nil "First Header "]]
        [:table-cell nil [:text nil "Second Header"]]]]
      [:table-body nil
       [:table-row nil
        [:table-cell nil [:text nil "Content Cell 1 "]]
        [:table-cell nil [:text nil "Content Cell 2"]]]
       [:table-row nil
        [:table-cell nil [:text nil "Content Cell 3 "]]
        [:table-cell nil [:text nil "Content Cell 4"]]]]]]

    ))