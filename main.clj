(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))

(->> "
def id(x)
  let y = x
  x * y
end
"
     lexer/tokenize
     parser/parse
     compiler/eval-js)
