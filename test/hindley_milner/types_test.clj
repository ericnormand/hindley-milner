(ns hindley-milner.types-test
  (:require [clojure.test :refer :all]
            [hindley-milner.syntax :refer :all]
            [hindley-milner.types :refer :all]))

(deftest infer-type-test

  (is (= :Integer (infer tenv 1)))
  (is (= :Boolean (infer tenv true)))
  (is (= :Boolean (infer tenv false)))
  (is (= :Integer (infer (assoc tenv 'v (forall [] :Integer))
                         'v)))
  (is (= :Integer (infer (assoc tenv 'v :Integer)
                         'v)))

  (let [[forall [var] [l x y]] (infer tenv '(fn x x))]
    (is (= l :Lambda))
    (is (= x y))
    (is (= x var)))

  (is (= :Boolean (infer tenv '(zero? 1))))
  (is (= [:Lambda :Integer :Integer] (infer tenv '(* 1))))
  (is (= :Integer (infer tenv '((* 1) 2))))

  (is (= :Integer (infer tenv '(let [a 1] a))))
  (is (= :Boolean (infer tenv '(let [a 10] (zero? a)))))

  (is (= :Integer (infer tenv '(let [id (fn x x)] (id 1)))))

  (is (= :Integer (infer tenv '(let [a 7]
                                 (let [b (fn x ((+ a) x))]
                                   (b 90))))))

  (is (= [:Tuple :Integer :Boolean]
         (infer tenv '((pair 1) false)))))

(deftest infer-type*-test
  (is (= [:Tuple :Integer :Boolean]
         (infer* tenv '(pair 1 false))))
  (let [[_ [_] [_ [_ _ x] y]]
        (infer* tenv '(fn [a]
                        (let [x (fn [b]
                                  (let [y (fn [c]
                                            (a 1))]
                                    (y 2)))]
                          (x 3))))]
    (is (not (nil? x)))
    (is (= x y)))
  )
