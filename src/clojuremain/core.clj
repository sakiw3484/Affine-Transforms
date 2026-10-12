;CSC344 Fall26
;Clojure Main-Affine Transforms
;Sakiyah Winston

(ns clojuremain.core)

;input (transform [[a b] [c d]] [x y])
;output
;     new-x = (+ (* a x) (* b y))
;     new-y = (+ (* c x) (* d y))
; [x y]
;
;helper fns for transform
;(+ (* a x) (* b y))
(defn add [a b]
  (if (and (number? a) (number? b))
    (+ a b)
    (list '+ a b)))

;helper fns for transform
;(* d y)
(defn multiply [a x]
  (if (and (number? a) (number? x))
    (* a x)
    (list '* a x)))


;main transform logic
; Input:[[a b] [c d]] [x y]
(defn transformExtended [matrix point]
  ;;handles nested transforms in point
  ;; p3
  (let [
        a (first (first matrix))
        b (second (first matrix))
        c (first (second matrix))
        d (second (second matrix))
        x (first point)
        y (second point)]

    ;builds vector [new-x new-y]
    ; new-x = (+ (* a x) (* b y))
    ;;new-y = (+ (* c x) (* d y))
    (vector
      (add (multiply a x) (multiply b y))
      (add (multiply c x) (multiply d y)))))

;handles nested transforms as 'list
(defn transform [exp]
  (if (and (list? exp)
           ;(seq? exp)
           (= 'transform (first exp)))
    (transformExtended (second exp)
               (transform (nth exp 2))) ;if point is another transform
    exp))


(defn simplifyExtend [exp]
  (if (list? exp)
    (let [op (first exp)
          arg1 (simplifyExtend (second exp))
          arg2 (simplifyExtend (nth exp 2))]

      (cond
        (= op '+)
        (cond
          ;(+ 0 x) => x
          ;(+ x 0) => x
          (= arg1 0) arg2
          (= arg2 0) arg1
          (and (number? arg1) (number? arg2)) (+ arg1 arg2)
          :else (list '+ arg1 arg2))

        ;(* 1 x) => x
        ;(* x 1) => x
        ;(* 0 x) => 0
        ;(* x 0) => 0
        (= op '*)
        (cond
          (or (= arg1 0) (= arg2 0)) 0
          (= arg1 1) arg2
          (= arg2 1) arg1
          (= arg1 -1) (list '- arg2)
          (= arg2 -1) (list '- arg1)
          (and (number? arg1) (number? arg2)) (* arg1 arg2)
          :else (list '* arg1 arg2))

        (= op '-)
        ;(- (- x)) => x
        (if (and (= 2 (count exp))
                 (list? arg1)
                 (= '- (first arg1))
                 (= 2 (count arg1)))
          (second arg1)
          (if (number? arg1)
            (- arg1)
            (list '- arg1)))

        :else exp))
    exp))

(defn simplify [exp]
  (if (vector? exp)
    (let [x (first exp)
          y (second exp)]
      (vector (simplifyExtend x) (simplifyExtend y)))))


(defn lookup
  [i m]
  (get m i i))

(defn bind-values [exp bindings]
  (cond
    ;if exp is a symbol, replace it with its value from bindings
    (symbol? exp)
    (lookup exp bindings)

    ;if exp is a vector, recursively bind values for each element
    (vector? exp)
    (map (fn [i]
           (bind-values i bindings))
         exp)

    ;if exp is a list, recursively bind values for each element
    ;apply list rebuilds the result as a list
    (list? exp)
    (apply list (map (fn [i]
                       (bind-values i bindings))
                     exp))

    ;; If exp is not a symbol, vector, or list, leave it unchanged
    :else exp))


(defn evalexp [exp bindings]
  (simplify(transform(bind-values exp bindings))))

(def p1 '(transform [[a 3] [0 0]] [x y]))
(def p2 '(transform [[1 0] [0 (+ x 3)]] [(* x 2) y]))
(def p3 '(transform [[0 0] [1 1]] (transform [[2 0] [0 2]] (transform [[-1 0] [0 -1]] [x 2]))))
(def p4 '(transform [[2 0] [0 1]] [(+ x 5) y]))
(def p5 '(transform [[1 0] [0 1]] [x y]))
(def p6 '(transform [[2 0] [0 1]] [x y]))
(def p7 '(transform [[2 0] [0 1]] [(+ x 5) y]))



;(transform '[[1 2] [3 4]] [5 6])
; ;(bind-values '{a 5, y 2} p1)