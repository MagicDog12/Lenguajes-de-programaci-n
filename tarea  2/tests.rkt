#lang play
(require "core.rkt")
(require "surface.rkt")

;; TESTS

;; Parte 1:

;; Agregando logs: primer intento

;; El log se inicializa como lista vacia al empezar el programa
(test log (box '()))
;; Testeamos si la salida de interpretar algo sin un print no modifica el log
(test (interp-g (num 5)) (result (numV 5) (box '())))
;; La salida de printear un numero agrega el numero al log
(test (interp-g (printn (num 4))) (result (numV 4) (box (cons 4 '()))))
(test log (box (cons 4'())))
;; La salida de printear una operacion agrega el resultado de la operacion al log
(test (interp-g (printn (add (num 4) (num 5)))) (result (numV 9) (box (cons 9 '()))))
;; Luego de interpretar algo que no sea print el log vuelve a ser lista vacia
(test (interp-g (num 5)) (result (numV 5) (box '())))
(test log (box '()))
;; Luego de interpretar dos veces seguidas el print de un numero se agregan al log en cadena
(test (interp-g (add (printn (num 3)) (printn (num 5)))) (result (numV 8) (box (cons 5 (cons 3 '())))))
(test log (box (cons 5 (cons 3 '()))))

