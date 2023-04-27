#lang play
#|
* Pregunta 1:

a) ¿Cuál es la utilidad del pattern-matching?

R: Es una forma más elegante y compacta de encontrar patrones en estructuras

b ¿Por qúe usar subtitución directa (subst) no es una opción realista en términos
de implementación? Indique el problema y argumentelo con un programa de ejemplo del
lenguaje visto en clase.

R: El problema es de eficiencia debido a que para sustituir directamente se busca
el idenficador en el cuerpo y hay que recorrer todo el cuerpo sustituyendo cada vez
que se sustituye pero también se puede ahorrar memoria
Un ejemplo es {with {x 2} {with {y 3} {with {z 4} {+ x y z}}}}

c) ¿Qué es scope estático y qué relación tiene con el concepto de ambiente?

R:Si al momento de aplicar una función se usa el ambiente actual se está usando scope dinámico
en cambio si se un ambiente vacio entonces se está usando scope estático

d) ¿Por qué puede resultar útil tener scope dinámico en un lenguaje? Ejemplifique.
R:

e) ¿Cómo determinaría si un lenguaje tiene scope estático o scope dinámico? Ilustre con ejemplos.

R: {define {f {x}} {+ x y}}
   {with {y 3}
         {f 20}}
Si el resultado es 30 entonces se usa scope dinámico, si da error es estático

f) 
|#

#|
* P2
Indique el o los identificadores libres existentes en la siguiente expresión:
{with {x {+ 5 6}}
      {+ {with {y 7} {+ x y}} {+ {with {z 8} {+ z y}} z}}}
en el with z el identificador libre es y
|#

#|
* P3
En clases se vió lo que es la currificación, que en simple es transformar la forma en
que una función recibe sus argumentos
|#
