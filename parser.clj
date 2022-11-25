(ns parser)
(require '[clojure.core.match :refer [match]])

(defn parse-int [val]
  {:type :int, :value (parse-long val)})

(defn parse-expr [tokens]
  (match tokens
    [{:type :int
      :value val}
     & rest-tokens] [(parse-int val), rest-tokens]
    [{:type :id
      :value name}
     & rest-tokens] [{:type :id-lookup, :name name} rest-tokens]
    :else (assert false (str "parse-expr not implemented for " tokens))))

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
    [{:type :=} & args-rest] [{:type :no-args} args-rest]))

(defn parse-def [tokens]
  (let [[name after-def]
        (match tokens
          [{:type :id, :value name} & after-def] [name after-def]
          :else (assert false (str "invalid def statement")))
        [args-ast after-args] (parse-fn-args-any after-def)
        [body-ast rest-tokens] (parse-expr after-args)]
    [{:type :def
      :args args-ast
      :name name
      :body body-ast}
     rest-tokens]))

(defn parse
  ([tokens] (parse tokens []))
  ([tokens ast-list]
   (cond
     (empty? tokens) ast-list
     :else
     (let [[ast-node rest-tokens]
           (match tokens
             [{:type :let} & rest] (parse-let rest)
             [{:type :def} & rest] (parse-def rest)
             :else (assert false (str "unknown statement " tokens)))
           ast-list (conj ast-list ast-node)]
       (parse rest-tokens ast-list)))))