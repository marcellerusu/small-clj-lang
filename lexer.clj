(ns lexer)

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
             #"[a-zA-Z]+" :id,
             #"=" :=,
             #"[0-9]+" :int})

(defn tokenize [program-str]
  (reduce
   (fn [{token-list :token-list, index :index} _]
     (cond
       (= index (count program-str)) {:token-list token-list, :index index}
       :else (let [rest-of-program (subs program-str index)
                   result (some (fn [[regex keyword]]
                                  (match-token regex rest-of-program keyword))
                                tokens)]
               (cond
                 (nil? result) {:token-list token-list, :index (+ index 1)}
                 :else {:token-list (conj token-list result),
                        :index (+ (count (:value result)) index)}))))
   {:token-list [], :index 0} program-str))

(def test-program "
let a = 10
")

(tokenize test-program)
