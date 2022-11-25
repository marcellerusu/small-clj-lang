(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))


(->> "
def add(a, b) = a
"
     lexer/tokenize
     parser/parse
     compiler/eval-js)
