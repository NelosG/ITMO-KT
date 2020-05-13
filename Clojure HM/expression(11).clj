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
(def evaluate (method :evaluate))
(def diff (method :diff))
(def operands (field :operands))

(def Constant)
(def ConstantPrototype
  (let [number (field :value)]
    {:toString (fn [this]
                 (let [value (number this)] (format "%.1f" value)))
     :evaluate (fn [this _]
                 (number this))
     :diff (fn [_ _] (Constant 0))}))

(defn Constant [number]
  {:prototype ConstantPrototype
   :value     number})

(def VariablePrototype
  (let  [name (field :value)]
    {:toString (fn [this]
                 (name this))
     :evaluate (fn [this id]
                 (id (name this)))
     :diff     (fn [this id]
                 (if (= (name this) id) (Constant 1) (Constant 0)))}))

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
    {:toString (fn [this]
                 (str "(" (symbol this) " "
                      (clojure.string/join " " (mapv toString (operands this)))
                      ")"))
     :evaluate (fn [this vars]
                 (apply (function this)
                        (mapv (fn [operand] (evaluate operand vars))
                              (operands this))))
     :diff     (fn [this var]
                 (howToDiff this var))}))

(defn create-operation
  [lexeme action howToDiff]
  (fn [& operands]
    {:prototype {:prototype  Operation
                 :lexeme   lexeme
                 :action   action
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
(def objectOperations
  {
   '+      Add
   '-      Subtract
   '*      Multiply
   '/      Divide
   'negate Negate
   })

(defn parseObjectExpression [expr]
  (cond
    (seq? expr) (apply (objectOperations (first expr)) (mapv parseObjectExpression (rest expr)))
    (number? expr) (Constant expr)
    :else (Variable (str expr))))

(def parseObject
  (comp parseObjectExpression read-string))