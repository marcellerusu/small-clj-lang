(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))


(-> "
def id(x) = x + x
"
    lexer/tokenize
    parser/parse
    compiler/eval-js)
