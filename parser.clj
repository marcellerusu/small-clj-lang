(ns parser)
(require '[clojure.core.match :refer [match]])

(declare ^:private parse-expr)
(declare ^:private parse-until)

(defn- p-result [ast-node tokens]
  [ast-node tokens])

(defn- p-into [partial-ast-node keyword]
  (fn [[ast-result tokens]]
    (p-result (assoc partial-ast-node keyword ast-result) tokens)))

(defn- p-map [f]
  #(let [[ast tokens] %
         ast (f ast)]
     (p-result ast tokens)))

(defn- p-then [f]
  #(let [[ast tokens] %]
     (f ast tokens)))

(defn- p-pipe [result & pipe-fns]
  (let [[first-ast tokens] result]
    (loop [ast first-ast, rest-tokens tokens, fns pipe-fns]
      (let [f (first fns)
            rest-fns (drop 1 fns)]
        (cond
          (nil? f) (p-result ast rest-tokens)
          :else (let [[r t] (f [ast rest-tokens])]
                  (recur r t rest-fns)))))))

(defn- parse-int [val rest-tokens]
  (p-result {:type :int, :value (parse-long val)}
            rest-tokens))

(defn- parse-math-operand [lhs-expr op-type tokens]
  (p-pipe
   (parse-expr tokens)
   (p-into {:type :num-op, :lhs lhs-expr, :op-type op-type} :rhs)))

(defn- parse-secondary-ops [lhs tokens]
  (let [result
        (match tokens
          [{:type :plus} & rest-tokens] (parse-math-operand lhs :plus rest-tokens)
          [{:type :minus} & rest-tokens] (parse-math-operand lhs :minus rest-tokens)
          [{:type :star} & rest-tokens] (parse-math-operand lhs :mult rest-tokens)
          [{:type :div} & rest-tokens] (parse-math-operand lhs :div rest-tokens)
          :else nil)]
    (cond
      (nil? result) (p-result lhs tokens)
      :else (apply parse-secondary-ops result))))

(defn- parse-id-lookup [name rest-tokens]
  [{:type :id-lookup, :name name} rest-tokens])

(defn- parse-arr-expr [tokens]
  (loop [rest-tokens tokens, exprs []]
    (let [[expr rest]
          (match rest-tokens
            [{:type :comma} & rest] [:comma rest]
            [{:type :close-sq} & rest] [nil rest]
            :else (parse-expr rest-tokens))]
      (cond
        (nil? expr) (p-result {:type :arr, :exprs exprs} rest)
        (= expr :comma) (recur rest exprs)
        :else (recur rest (conj exprs expr))))))

(defn- parse-expr [tokens]
  (p-pipe
   (match tokens
     [{:type :int, :value val} & rest-tokens] (parse-int val rest-tokens)
     [{:type :id, :value name} & rest-tokens] (parse-id-lookup name rest-tokens)
     [{:type :open-sq} & rest-tokens] (parse-arr-expr rest-tokens))
   (p-then parse-secondary-ops)))

(defn- parse-let [name tokens]
  (let [[name body] (match tokens [{:type :=} & body] [name body])
        [expr rest-tokens] (parse-expr body)]
    (p-result {:type :let, :name name, :expr expr}
              rest-tokens)))

(defn- parse-comma-seperated-ids [tokens end-token]
  (loop [rest-tokens tokens, ids []]
    (let [[name rest]
          (match rest-tokens
            [{:type :id, :value name} {:type :comma} & rest] [name rest]
            [{:type :id, :value name} & rest] [name rest]
            [{:type end-token} & rest] [nil rest])]
      (cond
        (nil? name) (p-result ids rest)
        :else (recur rest (conj ids name))))))

(defn- parse-fn-args [tokens]
  (match tokens
    [{:type :open-p} & args-rest] (parse-comma-seperated-ids args-rest :close-p)
    [{:type :=} & args-rest] (p-result [] args-rest)))

(defn- parse-def-name [tokens]
  (match tokens
    [{:type :id, :value name} & after-def] (p-result name after-def)))

(defn- parse-def-multiline-body [tokens]
  (let [[body rest] (parse-until tokens :end)
        rest (match rest
               [{:type :end} & rest] rest
              ;; in case we're at the end of the file `& rest` expects it to be a non empty list
               [{:type :end}] [])]

    (p-result body rest)))

(defn- parse-single-line-fn-return [tokens]
  (p-pipe
   (parse-expr tokens)
   (p-into {:type :return} :expr)
   (p-map vector)))

(defn- parse-def-body [tokens]
  (match tokens
    [{:type :=} & after-eq] (parse-single-line-fn-return after-eq)
    :else (parse-def-multiline-body tokens)))

(defn- insert-implicit-returns [def-body]
  (let [body-without-last (pop def-body)
        last-elem (last def-body)
        last-elem (case (:type last-elem)
                    :return last-elem
                    {:type :return, :expr last-elem})]

    (conj body-without-last last-elem)))

(defn- parse-def [tokens]
  (let [[name tokens-after-id] (parse-def-name tokens)
        [args-ast tokens-after-args] (parse-fn-args tokens-after-id)
        [body-ast rest-tokens] (parse-def-body tokens-after-args)
        body-ast (insert-implicit-returns body-ast)]

    (p-result {:type :def, :args args-ast, :name name, :body body-ast}
              rest-tokens)))

(defn- parse-return [tokens]
  (let [[expr rest-tokens] (parse-expr tokens)]

    (p-result {:type :return, :expr expr}
              rest-tokens)))

(defn- parse-arr-deconstruction [tokens]
  (let [[ids rest-tokens] (parse-comma-seperated-ids tokens :close-sq)
        [expr rest-tokens] (match rest-tokens
                             [{:type :=} & rest] (parse-expr rest))]

    (p-result {:type :let-arr, :ids ids, :expr expr}
              rest-tokens)))

(defn- parse-statement [tokens]
  (match tokens
    [{:type :let} {:type :id, :value name} & rest] (parse-let name rest)
    [{:type :let} {:type :open-sq} & tokens-after-open-sq] (parse-arr-deconstruction tokens-after-open-sq)
    [{:type :def} & rest] (parse-def rest)
    [{:type :return} & rest] (parse-return rest)
    :else (parse-expr tokens)))

(defn- parse-until [tokens end-token]
  (loop [rest-tokens tokens ast-list []]
    (assert (not-empty rest-tokens) "shouldn't have reached the end")
    (cond
      (-> rest-tokens first :type (= end-token)) (p-result ast-list rest-tokens)
      :else (let [[ast-node rest-tokens] (parse-statement rest-tokens)]
              (recur rest-tokens (conj ast-list ast-node))))))

(defn parse [tokens]
  (loop [rest-tokens tokens ast-list []]
    (cond
      (empty? rest-tokens) ast-list
      :else (let [[ast-node rest-tokens] (parse-statement rest-tokens)]
              (recur rest-tokens (conj ast-list ast-node))))))
