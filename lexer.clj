(ns lexer)

(defn- match-token [program-str {regex :regex keyword :keyword}]
  (let [match-result (re-matcher regex program-str)
        start? (try (-> match-result .find)
                    (-> match-result .start
                        (= 0))
                    (catch Exception _ nil))]
    (cond start? (-> match-result
                     .group
                     (#(assoc {:type keyword} :value %))))))

(def ^:private tokens
  [{:regex #"let\b"     :keyword :let}
   {:regex #"="         :keyword :=}
   {:regex #"[0-9]+"    :keyword :int}
   {:regex #"def\b"     :keyword :def}
   {:regex #"\("        :keyword :open-p}
   {:regex #"\)"        :keyword :close-p}
   {:regex #","         :keyword :comma}
   {:regex #"\+"        :keyword :plus}
   {:regex #"\-"        :keyword :minus}
   {:regex #"\*"        :keyword :star}
   {:regex #"\/"        :keyword :div}
   {:regex #"[a-zA-Z]+" :keyword :id}])

(defn tokenize [program-str]
  (loop [token-list [], rest-of-program program-str]
    (let [result (some #(match-token rest-of-program %) tokens)]
      (cond
        (empty? rest-of-program) token-list
        (nil? result) (recur token-list (subs rest-of-program 1))
        :else (recur (conj token-list result),
                     (subs rest-of-program (count (:value result))))))))
