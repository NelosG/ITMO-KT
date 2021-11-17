(defn common [f] (fn [& args]
                   (fn [m]
                     (apply f (mapv (fn [x] (x m)) args)))
                   ))
(defn constant [v] (fn [m] v))
(defn variable [v] (fn [m] (get m v)))
(def add (common +))
(def subtract (common -))
(def multiply (common *))
(def negate (common -))
(def divide (common (fn [x y] (/ (double x) (double y)))))
(def exp (common (fn [x] (Math/exp x))))
(def ln (common (fn [x] (Math/log (Math/abs x)))))
(def operations
  {'negate negate,
   '+ add,
   '- subtract,
   '* multiply,
   '/ divide,
   'ln ln,
   'exp exp})

(defn parse [str]
  (cond (number?  str) (constant str)
        (symbol? str) (variable (name str))
        :else (apply (operations (first str)) (mapv parse (rest str)))))

(defn parseFunction [str]
  (parse (read-string str)))