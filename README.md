# hindley-milner

An implementation of Algorithm W (and an interpreter) for a simple
lambda-calculus.

## Usage

To run the tests:

    lein test

To infer a type of an expression at the repl

    hindley-milner.types> (infer* tenv '(if true 1 2))

To evaluate an expression at the repl

    hindley-milner.eval> (interpret* env '(if true 1 2))

## Goals

* Implement Hindley-Milner in Clojure (DONE!)
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

Please see http://lispcast.com/Hindley-Miler-in-clojure for rationale.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
