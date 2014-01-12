(ns hindley-milner.syntax
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.walk :as walk]))

;; util
(defn vmap [f mp]
  (into {} (for [[k v] mp] [k (f v)])))

;; value code

(defn application?
  "Is this form a function application?
   Function applications are seqs whose first elements are not
   reserved for special forms."
  [x]
  (and (seq? x)
       (not (contains? #{'fn 'let} (first x)))))

(defn variable?
  "Is this form a variable?"
  [x]
  (symbol? x))

(defn lambda?
  "Is this form a lambda abstraction?
   Lambda abstractions are seqs with 'fn as the first element."
  [x]
  (and (seq? x)
       (= 'fn (first x))))

(defn let?
  "Is this form a let abstraction?
   Let abstractions are seqs with 'let as the first element."
  [x]
  (and (seq? x)
       (= 'let (first x))))

(defn atom?
  "Atoms are atomic values. Numbers, Booleans, Thunks, and Functions."
  [x]
  (or
   (number? x)
   (instance? java.lang.Boolean x)
   (instance? clojure.lang.IDeref x)
   (fn? x)))

(defn free-vars
  "Calculate the free variables of an expression."
  [exp]
  (cond
   (atom? exp)
   #{}

   (variable? exp)
   #{exp}

   (application? exp)
   (let [[fun arg] exp]
     (set/union (free-vars fun) (free-vars arg)))

   (lambda? exp)
   (let [[_ arg body] exp]
     (disj (free-vars body) arg))

   (let? exp)
   (let [[_ bindings body] exp
         bindings* (partition 2 bindings)
         vars (map first bindings*)
         vals (map second bindings*)]
     (set/difference
      (reduce set/union
              (free-vars body)
              (map free-vars vals))
      (set vars)))))

;; type code

(defn type-constant? [x]
  (keyword? x))

(defn type-variable? [x]
  (symbol? x))

(defn type-variable
  ([] (gensym))
  ([x]
     (let [[n] (string/split (name x) #"__")]
       (gensym (str n "__")))))

(defn type-application? [x]
  (vector? x))

(defn type-application [n ts]
  (vec (cons n ts)))

(defn type-env? [x]
  (map? x))

(defn type-env []
  {})

(defn type-scheme? [x]
  (and
   (seq? x)
   (= 'forall (first x))))

(defn free-types [x]
  (cond
   (type-constant? x)
   #{}
   (type-variable? x)
   #{x}
   (type-application? x)
   (let [[n & types] x]
     (reduce set/union #{} (map free-types types)))
   (type-scheme? x)
   (let [[_ vars t] x]
     (set/difference (free-types t) (set vars)))
   (type-env? x)
   (reduce set/union #{} (map free-types (vals x)))))

(defmacro forall [vars type]
  (if (seq vars)
    (let [fvs (free-types type)
          rplc (set/intersection (set fvs) (set vars))
          bindings (into {} (for [v rplc]
                              [v (type-variable v)]))]
      `(list ~''forall '~(vec (vals bindings))
             '~(walk/postwalk-replace bindings type)))
    type))

(defn compose-substitution [a b]
  (merge (vmap (partial walk/postwalk-replace a) b)
         a))

(defn transform-applications [exp]
  (walk/postwalk
   (fn [form]
     (if (application? form)
       (let [[f & args] form]
         (loop [largs args
                r f]
           (if (seq largs)
             (recur (rest largs) (list r (first largs)))
             r)))
       form))
   exp))

(defn transform-lambdas [exp]
  (walk/postwalk
   (fn [form]
     (if (lambda? form)
       (let [[_ args body] form]
         (loop [rargs (reverse args)
                f body]
           (if (seq rargs)
             (recur (rest rargs)
                    (list 'fn (first rargs) f))
             f)))
       form))
   exp))
