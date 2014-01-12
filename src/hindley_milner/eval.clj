(ns hindley-milner.eval
  (:refer-clojure :exclude [eval])
  (:require [hindley-milner.syntax :refer :all]))

(defn tramp
  "deref recursively."
  [x]
  (if (instance? clojure.lang.IDeref x)
    (recur (deref x))
    x))

(defn eval [env exp]
  (cond
   (atom? exp)
   exp

   (variable? exp)
   (if (contains? env exp)
     (get env exp)
     (throw (ex-info (str exp " is not defined.") {})))

   (application? exp)
   (let [[f arg] exp
         f* (eval env f)
         arg* (eval env arg)]
     (delay ((tramp f*) arg*)))

   (lambda? exp)
   (let [[_ arg body] exp
         fv (free-vars body)
         env* (select-keys env fv)]
     (fn [x]
       (eval (assoc env* arg x) body)))

   (let? exp)
   (let [[_ bindings body] exp
         binding-pairs (partition 2 bindings)
         promises (for [pair binding-pairs]
                    [pair (promise)])
         env* (reduce (fn [e [[var _] p]]
                        (assoc e var p))
                      env
                      promises)]
     (doseq [[[_ val] p] promises]
       (deliver p (eval env* val)))
     (eval env* body))

   :else
   (throw (ex-info (str "Not evalable: " exp "::" (type exp)) {:exp exp}))))

(defn interpret [env exp]
  (tramp (eval env exp)))

(defn interpret* [env exp]
  (->> exp
       transform-applications
       transform-lambdas
       (interpret env)))

(def env {'zero? (fn [x]
                   (zero? (tramp x)))
          'if (fn [test]
                (fn [then]
                  (fn [else]
                    (if (tramp test) then else))))
          '* (fn [a]
               (fn [b]
                 (* (tramp a) (tramp b))))
          'dec (fn [x]
                 (dec (tramp x)))
          'inc (fn [x]
                 (inc (tramp x)))
          'pair (fn [a]
                  (fn [b]
                    [(tramp a) (tramp b)]))})
