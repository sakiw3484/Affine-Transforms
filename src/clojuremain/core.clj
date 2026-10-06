;CSC344 Fall26
;Clojure Main-Affine Transforms
;Sakiyah Winston

(ns clojuremain.core)

;input (transform [[a b] [c d]] [x y])
;output
;     new-x = (+ (* a x) (* b y))
;     new-y = (+ (* c x) (* d y))
; [x y]
(defn transform [matrix point]
  [;check for variables new-x = (+ (* a x) (* b y))
   (if (and (number? (first (first matrix))) ;a
            (number? (second (first matrix))) ;b
            (number? (first point)) ; x
            (number? (second point))); y
     ;;if no var
     (+ (* (first (first matrix))
           (first point));(* a x)
        (* (second (first matrix))
           (second point))) ;(* b y)
     ; else have a variable somewhere
     (list '+
           (list '* (first (first matrix)) (first point));(* a x)
           (list '* (second (first matrix)) (second point));(* b y)
           ))

   ;check for variables new-y = (+ (* c x) (* d y))
   (if (and (number? (first (second matrix))) ;c
            (number? (second (second matrix))) ;d
            (number? (first point)) ; x
            (number? (second point))); y
     ;;if no var
     (+ (* (first (second matrix)) (first point));(* c x)
        (* (second (second matrix)) (second point))) ;(* d y)
     ; else have a variable somewhere
     (list '+
           (list '* (first (second matrix)) (first point));(* c x)
           (list '* (second (second matrix)) (second point))) ;(* d y)
     )])



(defn simplify [exp]
  (if (not (list? exp))
    exp

    (cond
      (= (first exp) '*)
      (simplify-multiply
        (simplify (second exp))
        (simplify (last exp)))

      (= (first exp) '+)
      (simplify-add
        (simplify (second exp))
        (simplify (last exp)))

      (= (first exp) '-)
      ; handle subtraction here

      :else
      exp)))

  (defn simplify-add [x y]
    (cond
      (= x 0) y
      (= y 0) x

      (and (number? x) (number? y))
      (+ x y)

      :else
      (list '+ x y)))

  (defn simplify-multiply [x y]
    (cond
      (= x 0) 0
      (= y 0) 0

      (= x 1) y
      (= y 1) x

      (and (number? x) (number? y))
      (* x y)

      :else
      (list '* x y)))

(defn bind-values [bindings exp]
  )

(defn evalexp [exp bindings] (simplify (bind-values bindings exp))
  )
