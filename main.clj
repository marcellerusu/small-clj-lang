(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))

(->> "
let [a, b] = [1, 2]
"
     lexer/tokenize
     parser/parse
     compiler/eval-js)
