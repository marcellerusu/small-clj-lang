(ns parser)
(require '[clojure.core.match :refer [match]])

(defn parse-int [val]
  {:type :int
   :value (parse-long val)})

(defn parse-math-operand [lhs-expr op-type tokens]
  (let [[rhs-expr rest-tokens] (parse-expr tokens)]
    [{:type :num-op
      :lhs lhs-expr
      :op-type op-type
      :rhs rhs-expr}
     rest-tokens]))

(defn parse-secondary-ops [lhs tokens]
  (let [result
        (match tokens
          [{:type :plus} & rest-tokens] (parse-math-operand lhs :plus rest-tokens)
          [{:type :minus} & rest-tokens] (parse-math-operand lhs :minus rest-tokens)
          [{:type :star} & rest-tokens] (parse-math-operand lhs :mult rest-tokens)
          [{:type :div} & rest-tokens] (parse-math-operand lhs :div rest-tokens)
          :else nil)]
    (cond
      (nil? result) [lhs tokens]
      :else (apply parse-secondary-ops result))))

(defn parse-expr [tokens]
  (let [[expr rest-tokens]
        (match tokens
          [{:type :int, :value val} & rest-tokens] [(parse-int val), rest-tokens]
          [{:type :id, :value name} & rest-tokens] [{:type :id-lookup, :name name} rest-tokens]
          :else (assert false (str "parse-expr not implemented for " tokens)))
        [expr rest-tokens] (parse-secondary-ops expr rest-tokens)]
    [expr rest-tokens]))

(defn parse-let [tokens]
  (let [[name body]
        (match tokens
          [{:type :id, :value name}
           {:type :=} & body] [name body]
          :else (assert false (str "invalid let statement" tokens)))
        [expr rest-tokens] (parse-expr body)]
    [{:type :let
      :name name
      :expr expr}
     rest-tokens]))

(defn parse-fn-args-some [tokens args]
  (let [[name rest]
        (match tokens
          [{:type :close-p} {:type :=} & rest] [nil rest]
          [{:type :id, :value arg-name} {:type :comma} & rest] [arg-name rest]
          [{:type :id, :value arg-name} & rest] [arg-name rest]
          :else (assert false (str "unknown fn-args pattern: " tokens)))]
    (cond
      (nil? name) [args rest]
      :else (parse-fn-args-some rest (conj args name)))))

(defn parse-fn-args-any [tokens]
  (match tokens
    [{:type :open-p} & args-rest] (parse-fn-args-some args-rest [])
    [{:type :=} & args-rest] [[] args-rest]))

(defn parse-def [tokens]
  (let [[name tokens-after-id]
        (match tokens
          [{:type :id, :value name} & after-def] [name after-def]
          :else (assert false (str "invalid def statement")))
        [args-ast tokens-after-args] (parse-fn-args-any tokens-after-id)
        [body-ast rest-tokens] (parse-expr tokens-after-args)]
    [{:type :def
      :args args-ast
      :name name
      :body body-ast}
     rest-tokens]))

(defn parse-statement [tokens]
  (match tokens
    [{:type :let} & rest] (parse-let rest)
    [{:type :def} & rest] (parse-def rest)
    :else (assert false (str "unknown statement " tokens))))

(defn parse [tokens]
  (loop [rest-tokens tokens ast-list []]
    (cond
      (empty? rest-tokens) ast-list
      :else (let [[ast-node rest-tokens] (parse-statement rest-tokens)]
              (recur rest-tokens (conj ast-list ast-node))))))
