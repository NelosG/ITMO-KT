
(defn proto-get [obj key]
  (cond
    (contains? obj key) (obj key)
    (contains? obj :prototype) (proto-get (obj :prototype) key)
    :else nil))

(defn proto-call [this key & args]
  (apply (proto-get this key) this args))

(defn field [key]
  (fn [this] (proto-get this key)))

(defn method [key]
  (fn [this & args] (apply proto-call this key args)))

(def toString (method :toString))
(def toStringSuffix (method :toStringSuffix))
(def evaluate (method :evaluate))
(def diff (method :diff))
(def operands (field :operands))

(def Constant)
(def ConstantPrototype
  (let [number (field :value)]
    {:toStringSuffix (fn [this]
                       (let [value (number this)] (format "%.1f" value)))
     :toString       (fn [this]
                       (let [value (number this)] (format "%.1f" value)))
     :evaluate       (fn [this _]
                       (number this))
     :diff           (fn [_ _] (Constant 0))}))

(defn Constant [number]
  {:prototype ConstantPrototype
   :value     number})
(defn frst [in] (clojure.string/lower-case (str (first (seq (str in))))))
(def VariablePrototype
  (let [name (field :value)]
    {:toStringSuffix (fn [this]
                       (name this))
     :toString       (fn [this]
                       (name this))
     :evaluate       (fn [this id]
                       (id (frst (name this))))
     :diff           (fn [this id]
                       (if (= (frst (name this)) id) (Constant 1) (Constant 0)))}))

(defn Variable [identifier]
  {:prototype VariablePrototype
   :value     identifier})

(def diff-of (fn [var this index] (diff ((operands this) index) var)))
(def operand-at (fn [this index] ((operands this) index)))
(def d-operands (fn [this var] (map (fn [operand] (diff operand var)) (operands this))))

(def Operation
  (let [operands (field :operands)
        symbol (field :lexeme)
        function (field :action)
        howToDiff (method :howToDiff)]
    {:toStringSuffix (fn [this]
                       (str "("
                            (clojure.string/join " " (mapv toStringSuffix (operands this)))
                            " " (symbol this) ")"))
     :toString       (fn [this]
                       (str "(" (symbol this) " "
                            (clojure.string/join " " (mapv toString (operands this)))
                            ")"))
     :evaluate       (fn [this vars]
                       (apply (function this)
                              (mapv (fn [operand] (evaluate operand vars))
                                    (operands this))))
     :diff           (fn [this var]
                       (howToDiff this var))}))

(defn create-operation
  [lexeme action howToDiff]
  (fn [& operands]
    {:prototype {:prototype Operation
                 :lexeme    lexeme
                 :action    action
                 :howToDiff howToDiff}
     :operands  (vec operands)}))

(def Add
  (create-operation '+ + (fn [this var] (apply Add (d-operands this var)))))

(def Subtract
  (create-operation '- - (fn [this var] (apply Subtract (d-operands this var)))))

(def Multiply
  (create-operation '* * (fn [this var] (Add (Multiply (operand-at this 0) (diff-of var this 1))
                                             (Multiply (operand-at this 1) (diff-of var this 0))))))
(def Divide
  (create-operation '/ (fn [x y] (/ x (double y)))
                    (fn [this var] (Divide (Subtract (Multiply (operand-at this 1) (diff-of var this 0))
                                                     (Multiply (operand-at this 0) (diff-of var this 1)))
                                           (Multiply (operand-at this 1) ((operands this) 1))))))
(def Negate
  (create-operation 'negate - (fn [this var] (apply Negate (d-operands this var)))
                    ))
;11 HM
;//////////////////////////////////////////////
(def Exp
  (create-operation 'exp (fn [x] (Math/exp x))
                    (fn [this var] (Multiply (Exp (operand-at this 0)) (diff-of var this 0)))
                    ))

(def Ln (create-operation 'ln (fn [x] (Math/log (Math/abs x)))
                          (fn [this var] (Multiply (Divide (Constant 1) (operand-at this 0)) (diff-of var this 0)))
                          ))
(def Log (create-operation '"//" (fn [x y] (/ (Math/log (Math/abs x)) (Math/log (Math/abs y))))
                           (fn [this var] (Multiply (Multiply (Divide (Constant 1) (operand-at this 0)) (Ln operand-at this 1)) (diff-of var this 0)))
                           ))
(def Pow (create-operation '"**" (fn [x y] (Math/pow x y))
                           (fn [this var] (Multiply (Pow (operand-at this 0) (- (operand-at this 0) 1)) (diff-of var this 0)))
                           ))
;////////////////////////
(def objectOperations {
                       "+"      Add
                       "-"      Subtract
                       "/"      Divide
                       "*"      Multiply
                       "negate" Negate
                       "exp"    Exp
                       "ln"     Ln
                       "**"     Pow
                       "//"     Log
                       })
(defn parseObjectExpression [expr]
  (cond
    (seq? expr) (apply (objectOperations (str (first expr))) (mapv parseObjectExpression (rest expr)))
    (number? expr) (Constant expr)
    :else (Variable (str expr))))

(def parseObject
  (comp parseObjectExpression read-string))








(defn -return [value tail] {:value value :tail tail})
(def -valid? boolean)
(def -value :value)
(def -tail :tail)
(defn _char [p] (fn [[c & cs]] (if (and c (p c)) (-return c cs))))
(defn _map [f result] (if (-valid? result) (-return (f (-value result)) (-tail result))))
(defn _either [a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) ar ((force b) str)))))
(defn +char [chars] (_char (set chars)))
(defn +map [f parser] (comp (partial _map f) parser))
(def +ignore (partial +map (constantly 'ignore)))
(defn iconj [coll value] (if (= value 'ignore) coll (conj coll value)))
(defn _empty [value] (partial -return value))
(defn _combine [f a b] (fn [str] (let [ar ((force a) str)] (if (-valid? ar) (_map (partial f (-value ar)) ((force b) (-tail ar)))))))
(defn +seq [& ps] (reduce (partial _combine iconj) (_empty []) ps))
(defn +seqf [f & ps] (+map (partial apply f) (apply +seq ps)))
(defn +or [p & ps] (reduce _either p ps))
(defn +star [p] (letfn [(rec [] (+or (+seqf cons p (delay (rec))) (_empty ())))] (rec)))
(defn +plus [p] (+seqf cons p (+star p)))
(defn +str [p] (+map (partial apply str) p))
(def *digit (+char "0123456789.-"))
(def *number (+map read-string (+str (+plus *digit))))
(defn +seqn [n & ps] (apply +seqf (fn [& vs] (nth vs n)) ps))
(def *space (+char " \t\n\r"))
(def *ws (+ignore (+star *space)))
(defn _parser [p] (fn [input] (-value ((_combine (fn [v _] v) p (_char #{\u0000})) (str input \u0000)))))




(declare !expression)

(defn !cut [p] (+seqn 0 *ws p *ws))
(def !var (+map #(Variable (str %)) (+str (!cut (+plus (+char "xyzXYZ"))))))
(def !num (+map #(Constant (read-string %)) (+str (!cut (+seq *number)))))
(def *skipBr (+ignore (!cut (+char "()"))))
(defn !literal [name] (apply +seqf str (mapv +char (mapv str (seq name)))))
(def *operation (+or (!literal "negate") (+char "+-/*")))
(def !word (!cut (+or !num !var (delay !expression))))
(def !unary (+map #((get objectOperations (str (get % 1))) (get % 0)) (+seq *skipBr !word *operation *skipBr)))
(def !binary (+map #((get objectOperations (str (get % 2))) (get % 0) (get % 1)) (+seq *skipBr !word !word *operation *skipBr)))
(def !expression (+or !binary !unary !num !var))
(def parseObjectSuffix (_parser (!cut (delay (+or !var !num !unary !binary)))))


