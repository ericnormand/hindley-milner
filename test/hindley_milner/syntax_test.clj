(ns hindley-milner.syntax-test
  (:require [clojure.test :refer :all]
            [hindley-milner.syntax :refer :all]))

(deftest lambda-test
  (is (lambda? '(fn [a] x))))

(deftest forall-test
  (is (= :Integer (forall [] :Integer)))
  (let [[forall [var] [l x y]] (forall [a] [:Lambda a a])]
    (is (= l :Lambda))
    (is (= x y))
    (is (= x var)))
  (let [[forall [a b] [l x y]] (forall [a b] [:Lambda a b])]
    (is (= l :Lambda))
    (is (= a x))
    (is (= b y))))

(deftest free-vars-test
  (is (= #{} (free-vars 1)))
  (is (= #{'a} (free-vars 'a)))
  (is (= #{'a 'b 'c} (free-vars '((a b) c))))
  (is (= #{'x} (free-vars '(fn a x))))
  (is (= #{'x 'y 'z 'c} (free-vars '(let [a x
                                          b y]
                                      (((z a) b) c))))))

(deftest free-types-test
  (is (= #{} (free-types :Int)))
  (is (= #{'x} (free-types 'x)))
  (is (= #{'a 'b} (free-types [:Xyz 'a 'b :Int])))
  (is (= #{} (free-types (forall [a b]
                                 [:Lambda a b]))))
  (is (= #{'b} (free-types (forall [a]
                                   [:Lambda a b]))))
  (is (= #{} (free-types (forall [a]
                                 :Int)))))

(deftest apply-substitution-test
)
