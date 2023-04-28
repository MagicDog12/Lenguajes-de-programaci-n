#lang play

;p1
;1: responde 0, el scope por defecto de racket es estático y la clausura guarda el valor de x en el momento de su definición. O sea, 0.
;2: respondería 42, la redefinición de x se propagaría hacia la función retornando el último valor con que x fue definido. O sea, 42.
;3: no puede ser función. La especificación del if nos indica que la rama true solo debe ejecutarse si
; la condición es verdadera, por otro lado la rama false debe ejecutarse solo si la condición es falsa.
; si tuvieramos una función (if c t f) con evaluación emprana, todos los argumentos reducirían antes de
; ejecutar el if. Por lo que se ejecutarían ambas ramas independiente de la condición.

;p2
; a) las clausuras se crean con el ambiente vacío en vez de con el ambiente actual y
; la condición de las expresiónes if-then-else no se interpreta

; b)
; > (with n 5 (with (f (fun x n)) (f 42)))
; > (if (+ 1 1) 1 1)

; c)
; > cambiar la linea 
; [ (fun id body) (closureV id body empty-env)] por 
; [ (fun id body) (closureV id body env)]
; y cambiar la linea
; ( if (num-zero? c)) por
; ( if (num-zero? (interp c env)))


;p3
;a {with {x 5} {with {y 24} {fun {x} x}}}
;b En la teoría no importa, porque efectivamente tiene derecho a conocer los valores de esos identificadores
; en la practica importa, porque ocupan espacio
;c guardar el ambiente sin los identificadores de (free-vars body) en el ambiente
