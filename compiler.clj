(ns compiler)
(require '[clojure.string :as string])

(defn eval-let [node]
  (str "let " (:name node) " = " (eval-node (:expr node))))

(defn indented [& strings]
  (->> strings
       (map #(str "  " %))
       (string/join ";\n")))

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
