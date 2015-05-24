(ns marksism.node-test
  (:require [clojure.test :refer :all]
            [marksism.node :refer :all]))

(deftest elavator-tests
  (is (liftable? (elevate ["foo"])))

  (are [coll expected]
    (= expected (flatten-nodes coll))

    [:foo :bar]
    [:foo :bar]

    [1 2 [3 4]]
    [1 2 [3 4]]

    [1 (elevate [2 3]) 4]
    [1 2 3 4]

    [(elevate [1 2 (elevate [3 4])]) 5 6]
    [1 2 3 4 5 6]

    [(elevate [1 2]) (elevate [3 4])]
    [1 2 3 4]
    ))

(deftest combine-adjacent-tests
  (are [pred combiner coll expected]
    (= expected (combine-adjacent pred combiner coll))

    string? str
    ["foo" "bar"]
    ["foobar"]

    string? str
    [1 "foo" "bar" 2 3]
    [1 "foobar" 2 3]

    string? str
    ["foo" " & " "bar"]
    ["foo & bar"]


    odd? +
    [0 1 1 2 3 5 8 13 21]
    [0 2 2 8 8 34]

    even? +
    [2 4 6 8]
    [20]

    some? str nil nil
    some? str [] []
    some? str (list) (list)

    string? str ["a"] ["a"]
    string? str [1] [1]
    ))