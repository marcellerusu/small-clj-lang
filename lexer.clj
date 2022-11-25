(ns lexer)
(require '[clojure.core.match :refer [match]])
(require '[clojure.string :as string])


(defn match-token [regex program-str keyword]
  (let [match-result (re-matcher regex program-str)
        start? (try (-> match-result .find)
                    (-> match-result .start
                        (= 0))
                    (catch Exception _ nil))]
    (cond start? (-> match-result
                     .group
                     (#(assoc {:type keyword} :value %))))))

(def tokens {#"let\b" :let,
             #"=" :=,
             #"[0-9]+" :int
             #"def\b" :def
             #"\(" :open-p
             #"\)" :close-p
             #"," :comma
             #"[a-zA-Z]+" :id})

(defn tokenize-single [rest-of-program]
  (some
   (fn [[regex keyword]]
     (match-token regex rest-of-program keyword))
   tokens))

(defn tokenize [program-str]
  (let [{tokens :token-list}
        (reduce
         (fn [{token-list :token-list, index :index} _]
           ;; TODO this cond shouldn't need to be here!
           (cond
             (>= index (count program-str)) {:token-list token-list, :index index}
             :else
             (let [rest-of-program (subs program-str index)
                   result (tokenize-single rest-of-program)]
               (cond
                 (nil? result) {:token-list token-list, :index (+ index 1)}
                 :else {:token-list (conj token-list result),
                        :index (+ (count (:value result)) index)}))))
         {:token-list [], :index 0} program-str)]
    tokens))

(def test-program "
let a = 10 let
")

(+ 1 2)

(tokenize test-program)
(def test-tokens [{:type :let, :value "let"}
                  {:type :id, :value "a"}
                  {:type :=, :value "="}
                  {:type :int, :value "10"}
                  {:type :let, :value "let"}])

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

(defn eval-let [node]
  (str "let " (:name node) " = " (eval-node (:expr node))))

(defn indented [& strings]
  (->> strings
       (map #(str "  " %))
       (string/join ";\n")))

(->> ["str"]
     (map #(str " " %)))

(indented "a" "b")


(defn eval-def [node]
  (println (:body node) " | " (eval-node (:body node)))
  (str
   "function "
   (:name node)
   "("
   (string/join ", " (:args node))
   ") {\n"
   (indented
    (str "return " (eval-node (:body node))))
   "\n}"))

(def args  ["a" "b"])
(string/join ", " args)

(defn eval-node [node]
  (case (:type node)
    :let (eval-let node)
    :int (str (:value node))
    :id-lookup (:name node)
    :def (eval-def node)))


(defn eval-js
  ([ast] (eval-js ast ""))
  ([ast output]
   (let [statement (first ast)
         result-js (eval-node statement)
         js (cond
              (empty? output) result-js
              :else (str output ";\n" result-js))
         rest (drop 1 ast)]

     (cond
       (or (nil? result-js) (empty? rest)) js
       :else (eval-js rest output)))))

(->> "
def add(a, b) = 10
"
     tokenize
     parse
     eval-js
     (spit "output.js"))


