#lang play
(require "core.rkt")
(require "surface.rkt")
(require "env.rkt")

;; TESTS

;; Parte 1:

;; Agregando logs: primer intento

;; El log se inicializa como lista vacia al empezar el programa
;;(test log (box '()))
;; Testeamos si la salida de interpretar algo sin un print no modifica el log
;;(test (interp-g (num 5)) (result (numV 5) (box '())))
;; La salida de printear un numero agrega el numero al log
;;(test (interp-g (printn (num 4))) (result (numV 4) (box (cons 4 '()))))
;;(test log (box (cons 4'())))
;; La salida de printear una operacion agrega el resultado de la operacion al log
;;(test (interp-g (printn (add (num 4) (num 5)))) (result (numV 9) (box (cons 9 '()))))
;; Luego de interpretar algo que no sea print el log vuelve a ser lista vacia
;;(test (interp-g (num 5)) (result (numV 5) (box '())))
;;(test log (box '()))
;; Luego de interpretar dos veces seguidas el print de un numero se agregan al log en cadena
;;(test (interp-g (add (printn (num 3)) (printn (num 5)))) (result (numV 8) (box (cons 5 (cons 3 '())))))
;;(test log (box (cons 5 (cons 3 '()))))

;; Alcance dinámico: segundo intento

;; Testeamos si la salida de interpretar algo sin un print no modifica el log
(test (interp-p (num 5)) (result (numV 5) (box '())))
;; La salida de printear un numero agrega el numero al log
(test (interp-p (printn (num 4))) (result (numV 4) (box (cons 4 '()))))
;; La salida de printear una operacion agrega el resultado de la operacion al log
(test (interp-p (printn (add (num 4) (num 5)))) (result (numV 9) (box (cons 9 '()))))
;; Luego de interpretar dos veces seguidas el print de un numero se agregan al log en cadena
(test (interp-p (add (printn (num 3)) (printn (num 5)))) (result (numV 8) (box (cons 5 (cons 3 '())))))

;; Ahora comenzamos con los unit tests
;; Caso interp: num sin print
(test (interp-p (num 10)) (result (numV 10) (box '())))
;; Caso interp: fun sin print
(test (interp-p (fun 'y (num 7))) (result (closV 'y (num 7) empty-env) (box '())))
;; Caso interp: add sin print
(test (interp-p (add (num 10) (num 7))) (result (numV 17) (box '())))
;; Caso interp: if0 sin print
(test (interp-p (if0 (num 10) (num 7) (num 8))) (result (numV 8) (box '())))
(test (interp-p (if0 (num 0) (num 7) (num 8))) (result (numV 7) (box '())))
;; Caso interp: id sin print
(test/exn (interp-p (id 'x)) "env-lookup: free identifier: x")
;; Caso interp: app sin print
(test (interp-p (app (fun 'x (id 'x)) (num 7))) (result (numV 7) (box '())))
;; Caso interp: printn caso base con num
(test (interp-p (printn (num 1))) (result (numV 1) (box (cons 1 '()))))
;; Caso interp: fun con print
(test (interp-p (fun 'y (printn (num 7)))) (result (closV 'y (printn (num 7)) empty-env) (box '())))
;; Caso interp: add con print
(test (interp-p (printn (add (num 3) (num 7)))) (result (numV 10) (box (cons 10 '()))))
(test (interp-p (add (printn (num 3)) (num 7))) (result (numV 10) (box (cons 3 '()))))
(test (interp-p (add (num 3) (printn (num 7)))) (result (numV 10) (box (cons 7 '()))))
(test (interp-p (add (printn (num 3)) (printn (num 7)))) (result (numV 10) (box (cons 7 (cons 3 '())))))
;; Caso interp: if0 con print (rama del if)
(test (interp-p (if0 (printn (num 0)) (num 7) (num 8))) (result (numV 7) (box (cons 0 '()))))
(test (interp-p (if0 (printn (num 0)) (printn (num 7)) (num 8))) (result (numV 7) (box (cons 7 (cons 0 '())))))
(test (interp-p (if0 (printn (num 0)) (printn (num 7)) (printn (num 8)))) (result (numV 7) (box (cons 7 (cons 0 '())))))
(test (interp-p (if0 (num 0) (printn (num 7)) (num 8))) (result (numV 7) (box (cons 7 '()))))
(test (interp-p (if0 (num 0) (printn (num 7)) (printn (num 8)))) (result (numV 7) (box (cons 7 '()))))
;; Caso interp: if0 con print (rama del else)
(test (interp-p (if0 (printn (num 10)) (num 7) (num 8))) (result (numV 8) (box (cons 10 '()))))
(test (interp-p (if0 (printn (num 10)) (num 7) (printn (num 8)))) (result (numV 8) (box (cons 8 (cons 10 '())))))
(test (interp-p (if0 (printn (num 10)) (printn (num 7)) (printn (num 8)))) (result (numV 8) (box (cons 8 (cons 10 '())))))
(test (interp-p (if0 (num 10) (num 7) (printn (num 8)))) (result (numV 8) (box (cons 8 '()))))
(test (interp-p (if0 (num 10) (printn (num 7)) (printn (num 8)))) (result (numV 8) (box (cons 8 '()))))
;; Caso interp: id con print
(test/exn (interp-p (printn (id 'x))) "env-lookup: free identifier: x")
;; Caso interp: app con print
(test (interp-p (app (fun 'x (printn (id 'x))) (num 7))) (result (numV 7) (box (cons 7 '()))))
(test (interp-p (app (fun 'x (id 'x)) (printn (num 7)))) (result (numV 7) (box (cons 7 '()))))
(test (interp-p (app (fun 'x (printn (id 'x))) (printn (num 8)))) (result (numV 8) (box (cons 8 (cons 8 '())))))
