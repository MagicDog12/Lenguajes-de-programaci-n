#lang play

#|

* Pregunta 1:

a) ¿Cuál es la utilidad del pattern-matching?

R: Es una forma más elegante y compacta de encontrar patrones en estructuras,
además que ayuda a la legibilidad y mantenibilidad del código. 

b ¿Por qúe usar subtitución directa (subst) no es una opción realista en términos
de implementación? Indique el problema y argumentelo con un programa de ejemplo del
lenguaje visto en clase.

R: El problema es de eficiencia debido a que para sustituir directamente se busca
el idenficador en el cuerpo y hay que recorrer todo el cuerpo sustituyendo cada vez
que se llama a está función pero también se puede ahorrar memoria
Un ejemplo es {with {x 2} {with {y 3} {with {z 4} {+ x y z}}}}

c) ¿Qué es scope estático y qué relación tiene con el concepto de ambiente?

R: El scope estático es el alcance léxico que tiene una variable dentro del código,
y ese alcance está determinado por la posición que tiene esa variable.
Si al momento de aplicar una función se usa el ambiente actual se está usando scope dinámico
en cambio si se usa un ambiente vacio entonces se está usando scope estático.


d) ¿Por qué puede resultar útil tener scope dinámico en un lenguaje? Ejemplifique.

R: El scope dinámico puede ser útil en ciertas situaciones donde se necesita acceder a variables
globales desde una función sin tener que pasar la variable como argumento.
Por ejemplo si tenemos una varibale global stdout que necesitamos usar dentro de una función que
se usa dentro de otra función, en este caso es ineficiente pasarle ese argumento a la primera
función sabiendo que solo se usar para pasarselo como argumento a la siguiente función.

e) ¿Cómo determinaría si un lenguaje tiene scope estático o scope dinámico? Ilustre con ejemplos.

R: Para analzar el scope de un lenguaje podemos analizar la respuesta del siguiente programa:
 {define {f {x}} {+ x y}}
   {with {y 3}
         {f 20}}
Si el resultado es 30 entonces se usa scope dinámico, si lanza error de
 identificador libre se usa scope estático.

f) ¿Por qué en un lenguaje con funciones de primera clase y substitución directa
(no diferida) no es necesario introducir una noción de clausura para preservar
el scope estático?

R: En un lenguaje con substitución directa, el valor de una variable se puede reemplazar
directamente en el lugar donde se usa, sin necesidad de utilizar una clausura.
Y en un lenguaje con funciones de primera clase, las funciones se pueden pasar
como argumentos, devolver como resultado y asignar a variables, lo que significa que el
alcance de la función se puede determinar en tiempo de ejecución.

|#

#|

* Pregunta 2:

Indique el o los identificadores libres existentes en la siguiente expresión:
{with {x {+ 5 6}}
      {+ {with {y 7} {+ x y}} {+ {with {z 8} {+ z y}} z}}}

R: El'y dentro de {+ z y} porque el 'y está definido solo en la otra rama del with
y el 'z del final porque el 'z está definido solo dentro del with adyacente.

|#


#|

* Pregunta 3:

En clases se vió lo que es la currificación, que en simple es transformar la forma en
que una función recibe sus argumentos, por ejemplo (f arg1 arg2) al ser currificada,
se aplica como ((f arg1) arg2)

a) Defina la función (curry n f), que currifica una función de n argumentos.

|#
(define (curry n f [lista '()])
  (if (zero? n)
      (apply f lista)
      (lambda (x) (curry (- n 1) f (append lista (list x))))))

;; Ejemplo 1:
(define (suma x y z) (+ x y z))
(test (suma 1 2 3) 6)
(define (suma-curry) (curry 3 suma))
(test ((((suma-curry) 2) 4) 6) 12)
;; Ejemplo 2:
(define (sum0) (+ 0 0))
(test (sum0) 0)
(define (sum0-curry) (curry 0 sum0))
(test (sum0-curry) 0)
#|

b) Defina la función (uncurry-2 f) que reciba una función currificada de 2 argumentos,
y devuelve la misma función tal que reciba ambos argumentos simultáneamente.

R:

|#
