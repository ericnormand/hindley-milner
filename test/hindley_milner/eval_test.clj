(ns hindley-milner.eval-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [hindley-milner.eval :refer :all]))

(deftest tramp-test
  (is (= 1 (tramp (delay (delay (delay (future 1))))))))

(deftest interpret-test
  (is (= 1 (interpret env 1)))
  (is (= 1 (interpret (assoc env 'v 1) 'v)))
  (is (= 1 (interpret env '(inc 0))))
  (is (fn? (interpret env '(fn a a))))
  (is (= 1 (interpret env '((fn a a) 1))))
  (is (= 1 (interpret env '((((fn a (fn b (fn c c))) 3) 2) 1))))
  (is (= 1 (interpret env '(let [id (fn a a)]
                             (id 1)))))
  (is (= 1 (interpret env '(let [id (fn a a)
                                 b 1]
                             (id b)))))
  (is (= 1 (interpret env '(let [c (dec b)
                                 id (fn a a)
                                 b 2]
                             (id c))))))
(deftest interpret*-test
  (is (= 1 (interpret* env 1)))
  (is (= 1 (interpret* (assoc env 'v 1) 'v)))
  (is (= 1 (interpret* env '(inc 0))))
  (is (fn? (interpret* env '(fn [a] a))))
  (is (= 1 (interpret* env '((fn [a] a) 1))))
  (is (fn? (interpret* env '((fn [a b c] c) 3 2))))
  (is (= 1 (interpret* env '((fn [a b c] c) 3 2 1))))
  (is (= 1 (interpret* env '(let [f (fn [a b c] c)]
                              (f 3 2 1)))))
  (is (= 1 (interpret* env '(let [id (fn [a] a)]
                              (id 1)))))
  (is (= 1 (interpret* env '(let [id (fn [a] a)
                                  b 1]
                              (id b)))))
  (is (= 1 (interpret* env '(let [c (dec b)
                                  id (fn [a] a)
                                  b 2]
                              (id c)))))  )
