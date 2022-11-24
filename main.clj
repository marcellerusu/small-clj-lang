(ns main)

(def tokens {#"let\b" :let,
             #"[a-zA-Z]+" :id,
             #"=" :=,
             #"[0-9]+" :int})

(defn match-token [regex program-str keyword]
  (let [match-result (re-matcher regex program-str)
        start? (try (-> match-result .find)
                    (-> match-result .start
                        (= 0))
                    (catch Exception _ nil))]
    (cond start? (-> match-result
                     .group
                     (#(assoc {:type keyword} :value %))))))

(match-token #"let\b" "let" :let)

(defn tokenize [program-str]
  (reduce
   (fn [{token-list :token-list, index :index} _]
     (cond
       (= index (count program-str)) {:token-list token-list, :index index}
       :else (let [rest-of-program (subs program-str index)
                   result (or
                           (match-token #"let\b" rest-of-program :let)
                           (match-token #"=" rest-of-program :=)
                           (match-token #"[a-zA-Z]+" rest-of-program :id)
                           (match-token #"[0-9]+" rest-of-program :num))
                   next-index (cond
                                (nil? result) (+ index 1)
                                :else (+ (count (:value result)) index))
                   new-token-list (cond
                                    (nil? result) token-list
                                    :else (conj token-list result))]
               {:token-list new-token-list
                :index next-index})))
   {:token-list [], :index 0} program-str))

(def test-program "
let a = 10
")

(tokenize test-program)

(-> (re-matcher #"let" test-program)
    (#(do
        (-> % .find)
        (-> % .group))))



(def result (re-matcher #"let\b" test-program))
(-> result .group)

(re-find #"ldet\b" test-program)
