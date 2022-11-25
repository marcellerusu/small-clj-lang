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


