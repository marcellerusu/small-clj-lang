(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))

(->> "
let [x] = 1
"
     lexer/tokenize
     parser/parse
     compiler/eval-js)
