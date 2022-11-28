(ns compiler
  (:require [clojure.string :as string]))

(declare ^:private eval-node)
(declare eval-js)

(defn- eval-let [{:keys [name expr]}]
  (str "let " name " = " (eval-node expr)))

(defn- eval-def [{:keys [name args body]}]
  (let [args-js (string/join ", " args)
        body-js (->> body
                     (map eval-node)
                    ;; TODO: non-hardcoded indentation level!
                     (string/join ";\n  ")
                     (str "  "))]
    (str "function " name "(" args-js ") {\n" body-js "\n}")))

(defn- eval-num-op [{:keys [op-type lhs rhs]}]
  (let [js-op (case op-type
                :plus "+"
                :minus "-"
                :mult "*"
                :div "/")]
    (str (eval-node lhs) " " js-op " " (eval-node rhs))))

(defn- eval-return [{expr :expr}]
  (str "return " (eval-node expr)))

(defn- eval-let-arr [{:keys [ids expr]}]
  (str "let [" (string/join ", " ids) "] = " (eval-node expr)))

(defn- eval-node [node]
  {:pre [(map? node)]}

  (case (:type node)
    :let (eval-let node)
    :let-arr (eval-let-arr node)
    :int (str (:value node))
    :id-lookup (:name node)
    :def (eval-def node)
    :num-op (eval-num-op node)
    :return (eval-return node)))

(defn eval-js
  ([ast] (eval-js ast ""))
  ([ast output]
   {:pre (vector? ast)}
   (let [statement (first ast)
         result-js (eval-node statement)
         js (cond
              (empty? output) result-js
              :else (str output ";\n" result-js))
         rest (drop 1 ast)]
     (cond
       (or (nil? result-js) (empty? rest)) js
       :else (eval-js rest js)))))
