(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))

(-> "
def id(x)
  return x * x
end
"
    lexer/tokenize
    parser/parse
    compiler/eval-js)
