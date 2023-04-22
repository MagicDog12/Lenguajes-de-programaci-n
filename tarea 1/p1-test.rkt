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

;; Tests avanzados:
;; Programa de Ejemplo con ningun argumento en el define
(test (run '{{define {up2} {+ 2 11}}
             {up2}}) (numV 13))
;; Programa de Ejemplo con más de un argumento en el define
(test (run '{{define {add3 x y z} {+ {+ x y} z}}
             {add3 2 3 4}}) (numV 9))
;; Programa de Ejemplo 1
(test (run '{ 
             {define {sum x y z} {+ x {+ y z}}}
             {define {cadr x} {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}} }
             })
      (numV 13))
;; Programa de Ejemplo 2
(test (run '{ 
             {with {{x 5} {y 23} {z {cons 11 -3}}}
                   z}
             })
      (pairV (numV 11) (numV -3)))
;; Programa de Ejemplo 3
(test (run '{ 
             {define {triple x} {+ x {+ x x}}}
             {define {add2 x} {+ 2 x}}
             {add2 {triple 2}}
             })
      (numV 8))
;; Programa de Ejemplo 4
(test (run '{ 
             {with {{x 3} {y {+ 1 2}}}
                   {if {= x y} x y}}
             })
      (numV 3))

;; Tests con Excepciones:

;; Casos en que esperabamos un numero y le damos un numero:
;; caso: add1
(test (run '{{add1 0}}) (numV 1))
;; caso: +
(test (run '{{+ 0 0}}) (numV 0))
;; caso: <
(test (run '{{< 0 0}}) (boolV #f))
;; caso: =
(test (run '{{= 0 0}}) (boolV #t))

;; Casos en que esperabamos un numero y le damos un bool:
;; caso: add1
(test/exn (run '{{add1 #t}}) "Runtime type error: expected Number found Bool")
;; caso 1: +
(test/exn (run '{{+ 0 #t}}) "Runtime type error: expected Number found Bool")
;; caso 2: +
(test/exn (run '{{+ #f 0}}) "Runtime type error: expected Number found Bool")
;; caso 3: +
(test/exn (run '{{+ #f #t}}) "Runtime type error: expected Number found Bool")
;; caso 1: <
(test/exn (run '{{< #t 0}}) "Runtime type error: expected Number found Bool")
;; caso 2: <
(test/exn (run '{{< 0 #t}}) "Runtime type error: expected Number found Bool")
;; caso 3: <
(test/exn (run '{{< #t #t}}) "Runtime type error: expected Number found Bool")
;; caso 1: =
(test/exn (run '{{= #t 0}}) "Runtime type error: expected Number found Bool")
;; caso 2: =
(test/exn (run '{{= 0 #t}}) "Runtime type error: expected Number found Bool")
;; caso 3: =
(test/exn (run '{{= #t #t}}) "Runtime type error: expected Number found Bool")

;; Casos en que esperabamos un numero y le damos un par:
;; caso: add1
(test/exn (run '{{add1 {cons 0 0}}}) "Runtime type error: expected Number found Pair")
;; caso 1: +
(test/exn (run '{{+ 0 {cons 1 2}}}) "Runtime type error: expected Number found Pair")
;; caso 2: +
(test/exn (run '{{+ {cons 1 2} 0}}) "Runtime type error: expected Number found Pair")
;; caso 3: +
(test/exn (run '{{+ {cons 1 2} {cons 1 2}}}) "Runtime type error: expected Number found Pair")
;; caso 1: <
(test/exn (run '{{< {cons 1 2} 0}}) "Runtime type error: expected Number found Pair")
;; caso 2: <
(test/exn (run '{{< 0 {cons 0 0}}}) "Runtime type error: expected Number found Pair")
;; caso 3: <
(test/exn (run '{{< {cons 1 2} {cons 0 0}}}) "Runtime type error: expected Number found Pair")
;; caso 1: =
(test/exn (run '{{= {cons 1 2} 0}}) "Runtime type error: expected Number found Pair")
;; caso 2: =
(test/exn (run '{{= 0 {cons 0 0}}}) "Runtime type error: expected Number found Pair")
;; caso 3: =
(test/exn (run '{{= {cons 1 2} {cons 0 0}}}) "Runtime type error: expected Number found Pair")

;; Casos en que esperabamos un booleano y le damos un booleano:
;; caso: !
(test (run '{{! #t}}) (boolV #f))
;; caso: &&
(test (run '{{&& #t #f}}) (boolV #f))
;; caso: ||
(test (run '{{|| #t #f}}) (boolV #t))

;; Casos en que esperabamos un booleano y le damos un numero:
;; caso: !
(test/exn (run '{{! 1}}) "Runtime type error: expected Bool found Number")
;; caso 1: &&
(test/exn (run '{{&& 3 #t}}) "Runtime type error: expected Bool found Number")
;; caso 2: &&
(test/exn (run '{{&& #t 1}}) "Runtime type error: expected Bool found Number")
;; caso 3: &&
(test/exn (run '{{&& 3 1}}) "Runtime type error: expected Bool found Number")
;; caso 1: ||
(test/exn (run '{{|| 3 #t}}) "Runtime type error: expected Bool found Number")
;; caso 2: ||
(test/exn (run '{{|| #f 1}}) "Runtime type error: expected Bool found Number")
;; caso 3: ||
(test/exn (run '{{|| 3 1}}) "Runtime type error: expected Bool found Number")

;; Casos en que esperabamos un booleano y le damos un par:
;; caso: !
(test/exn (run '{{! {cons 1 2}}}) "Runtime type error: expected Bool found Pair")
;; caso 1: &&
(test/exn (run '{{&& {cons 1 2} #t}}) "Runtime type error: expected Bool found Pair")
;; caso 2: &&
(test/exn (run '{{&& #t {cons 1 2}}}) "Runtime type error: expected Bool found Pair")
;; caso 3: &&
(test/exn (run '{{&& {cons 1 2} {cons 1 2}}}) "Runtime type error: expected Bool found Pair")
;; caso 1: ||
(test/exn (run '{{|| {cons 0 4} #t}}) "Runtime type error: expected Bool found Pair")
;; caso 2: ||
(test/exn (run '{{|| #t {cons 0 4}}}) "Runtime type error: expected Bool found Pair")
;; caso 3: ||
(test/exn (run '{{|| {cons 0 4} {cons 0 4}}}) "Runtime type error: expected Bool found Pair")

;; Casos en que esperabamos un par y le damos un par:

;; Casos en que esperabamos un par y le damos un numero:

;; Casos en que esperabamos un par y le damos un booleano:
