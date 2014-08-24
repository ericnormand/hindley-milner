# hindley-milner

An implementation of Algorithm W (and an interpreter) for a simple
lambda-calculus.

## Assumptions

You have installed [Leiningen](http://leiningen.org/#install) and [Git](http://git-scm.com/book/en/Getting-Started-Installing-Git)

## Getting Set Up
Clone the repository:

    git clone https://github.com/ericnormand/hindley-milner
    
    cd hindley-milner
    
    lein repl
    user>

    user> (ns user
            (:refer-clojure :exclude [eval])
            (:require [hindley-milner.syntax :refer :all]
			          [hindley-milner.eval :refer :all]
			          [hindley-milner.types :refer :all]))


## Usage - Syntax

    user> '(fn [a] x)
    
    (fn [a] x)

    user> (lambda? '(fn [a] x))
    
    true

    user> (free-vars '(let [a x
                   b y]
                (((z a) b) c)))
    
    #{'x 'y 'z 'c}

## Usage - Evaluation

    user> (interpret env 1)
    
    1

    user> (interpret env 
            '(let [c (dec b) 
    		   id (fn a a) 
    		   b 2] 
    		(id c)))
    		
    1

## Usage - Type Inference

    user> (infer tenv 1)

    :Integer

    user> (infer* tenv '(if true 1 2))
    
    [:Lambda :Integer]
    
    
    user> (let [[_ [_] [_ [_ _ x] y]]
            (infer* tenv '(fn [a]
                            (let [x (fn [b]
                                      (let [y (fn [c]
                                                (a 1))]
                                        (y 2)))]
                              (x 3))))]
            (= x y))

    true
    

## Usage - Running Tests

To run the tests:

    lein test


## Goals

Pull requests welcome!

* Implement Hindley-Milner in Clojure (DONE!)
* Learn and teach about type systems
* Discover similarities between Lisp's meta-circular `eval` 
  and syntax-directed HM.

### Non-goals

* To type check Clojure.

## TODO

* Add support for inferencing multiple bindings in a `let`.
* Add negative tests
* Docstrings

## License

Copyright © 2014 Eric Normand

Please see http://lispcast.com/Hindley-Milner-in-Clojure for rationale.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
