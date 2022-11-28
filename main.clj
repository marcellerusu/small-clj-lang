(ns main
  (:require lexer)
  (:require parser)
  (:require compiler))

(->> "
def add(a b)
  let [a b] = 10
  a + b
end
"
     lexer/tokenize
     parser/parse
     compiler/eval-js)
