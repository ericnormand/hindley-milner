(ns hindley-milner.types
  (:require
   [clojure.set :as set]
   [clojure.walk :as walk]
   [hindley-milner.syntax :refer :all]))

(defn generalize
  "Convert a type into a type scheme by converting free type variables
  into existential variables. Has no effect if there are no free
  variables."
  [env t]
  (let [vars (vec (set/difference (free-types t) (free-types env)))]
    (if (seq vars)
      (list 'forall vars t)
      t)))

(defn instantiate
  "Convert a type scheme into a type by replacing existential
   variables with unique, concrete type variables. Has no effect on
   other types."
  [t]
  (if (type-scheme? t)
    (let [[_ vars t*] t
          subst (zipmap vars (map type-variable vars))]
      (walk/postwalk-replace subst t*))
    t))

(defn vbind [v t]
  (if (contains? (free-types t) v)
   (throw (ex-info (str "Recursive unification.") {}))
   {v t}))

(defn unify [t1 t2]
  (cond
   (= t1 t2)
   {}

   (and (type-application? t1)
        (type-application? t2))
   (let [[n1 & ts1] t1
         [n2 & ts2] t2]
     (cond
      (not= n1 n2)
      (throw (ex-info (str "Cannot unify two different type applications: " n1 " and " n2) {}))

      (not= (count ts1) (count ts2))
      (throw (ex-info (str "Cannot unify two type applications with different numbers of types: " t1 " & " t2) {}))

      :else
      (reduce (fn [subst [t1 t2]]
                (let [subst2 (unify (walk/postwalk-replace subst t1)
                                    (walk/postwalk-replace subst t2))]
                  (compose-substitution subst subst2)))
              {}
              (map vector ts1 ts2))))

   (type-variable? t1)
   (vbind t1 t2)

   (type-variable? t2)
   (vbind t2 t1)

   :else
   (throw (ex-info (str "Types do not unify: " t1 t2) {}))))

(defn infer-atom [exp]
  (cond
   (integer? exp)
   :Integer
   (instance? Boolean exp)
   :Boolean))

(defn infer-type [env exp]
  (cond
   (variable? exp)
   (if-let [t (get env exp)]
     [{} (instantiate t)]
     (throw (ex-info (str "Unbound variable: " exp) {})))

   (atom? exp)
   [{} (infer-atom exp)]

   (lambda? exp)
   (let [[_ arg body] exp
         arg-t (type-variable arg)
         env* (assoc env arg arg-t)
         [subst body-t] (infer-type env* body)]
     [subst
      (walk/postwalk-replace subst [:Lambda arg-t body-t])])

   (application? exp)
   (let [[fun arg] exp
         res-t (if (variable? fun)
                 (type-variable (str (name fun) "-result"))
                 (type-variable))
         [s1 fun-t] (infer-type env fun)
         [s2 arg-t] (infer-type (walk/postwalk-replace s1 env) arg)
         s3 (unify (walk/postwalk-replace s2 fun-t)
                   [:Lambda arg-t res-t])]
     [(reduce compose-substitution s3 [s2 s1])
      (walk/postwalk-replace s3 res-t)])

   ;; I still need to make it work with multiple bindings
   (let? exp)
   (let [[_ [var val] body] exp]
     (if (contains? (free-vars val) var)
       (let [tv (type-variable var)
             env* (assoc env var tv)
             [s1 t1] (infer-type env* val)
             s1* (unify tv t1)
             [s2 t2] (infer-type (walk/postwalk-replace
                                  (compose-substitution s1 s1*) env*) body)]
         [(compose-substitution s1 s2) t2])
       (let [[s1 t1] (infer-type env val)
             t* (generalize (walk/postwalk-replace s1 env) t1)
             env* (assoc env var t*)
             [s2 t2] (infer-type (walk/postwalk-replace s1 env*) body)]
         [(compose-substitution s1 s2) t2])))))

(def tenv {'zero? [:Lambda :Integer :Boolean]
           'if (forall [a]
                       [:Lambda :Boolean
                        [:Lambda a
                         [:Lambda a
                          [:Lambda a]]]])
           '+ [:Lambda :Integer
               [:Lambda :Integer :Integer]]
           '* [:Lambda :Integer
               [:Lambda :Integer :Integer]]
           'dec [:Lambda :Integer :Integer]
           'inc [:Lambda :Integer :Integer]
           'pair (forall [a b]
                         [:Lambda a
                          [:Lambda b [:Tuple a b]]])
           'empty? (forall [a]
                           [:Lambda [:List a] :Boolean])
           'rest (forall [a]
                         [:Lambda [:List a] [:List a]])})

(defn infer [env exp]
  (let [[subst t] (infer-type env exp)]
    (generalize env (walk/postwalk-replace subst t))))

(defn infer* [env exp]
  (->> exp
       transform-applications
       transform-lambdas
       (infer env)))
