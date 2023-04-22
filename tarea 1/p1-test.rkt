#lang play
(require "p1.rkt")
(print-only-errors #t)

;; Tests básicos:
;; recibe solo num
(test (run '{5}) (numV 5))
;; recibe solo bool
(test (run '{#t}) (boolV #t))
;; recibe solo cons
(test (run '{{cons 1 2}}) (pairV (numV 1) (numV 2)))
;; vemos si aplica add1
(test (run '{{add1 3}}) (numV 4))
;; vemos si aplica +
(test (run '{{+ 3 4}}) (numV 7))
;; vemos si aplica <
(test (run '{{< 3 4}}) (boolV #t))
(test (run '{{< 4 3}}) (boolV #f))
;; vemos si aplica =
(test (run '{{= 1 1}}) (boolV #t))
(test (run '{{= 2 3}}) (boolV #f))
;; vemos si aplica !
(test (run '{{! #t}}) (boolV #f))
(test (run '{{! #f}}) (boolV #t))
;; vemos si aplica &&
(test (run '{{&& #t #t}}) (boolV #t))
(test (run '{{&& #f #t}}) (boolV #f))
(test (run '{{&& #t #f}}) (boolV #f))
(test (run '{{&& #f #f}}) (boolV #f))
;; vemos si aplica ||
(test (run '{{|| #t #t}}) (boolV #t))
(test (run '{{|| #f #t}}) (boolV #t))
(test (run '{{|| #t #f}}) (boolV #t))
(test (run '{{|| #f #f}}) (boolV #f))
;; vemos si aplica fst
(test (run '{{fst {cons 1 2}}}) (numV 1))
;; vemos si aplica snd
(test (run '{{snd {cons 3 4}}}) (numV 4))
;; vemos si aplica if
(test (run '{{if #t {+ 1 2} {+ 3 4}}}) (numV 3))
(test (run '{{if #f {+ 1 2} {+ 3 4}}}) (numV 7))
;; vemos si aplica with
(test (run '{{with {{x 1}} {add1 x}}}) (numV 2))
(test (run '{{with {{x 1} {y 2}} {+ x y}}}) (numV 3))
(test (run '{{with {} {+ 1 2}}}) (numV 3))
;; vemos si aplica una funcion
(test (run '{{define {add2 x} {+ 2 x}}
             {add2 4}}) (numV 6))

;; Tests moderados:
;; Programa de Ejemplo con ningun argumento en el define
(test (run '{{define {up2} {+ 2 11}}
             {up2}}) (numV 13))
;; Programa de Ejemplo con más de un argumento en el define
(test (run '{{define {add3 x y z} {+ {+ x y} z}}
             {add3 2 3 4}}) (numV 9))

(test (run '{ ;; Programa de Ejemplo 1
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))

(test (run '{ ;; Programa de Ejemplo 2
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))

(test (run '{ ;; Programa de Ejemplo 3
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))

(test (run '{ ;; Programa de Ejemplo 4
             {with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))

