#lang play

#| P1. Preguntas Teóricas

a) ¿Es posible implementar funciones recursivas en el lenguaje visto en clases hasta ahora? ¿Por qué?
R: Si es posible, esto se debe a que tenemos un namespace distinto para las funciones (funs) y este es global,
   es decir, esta siempre disponible para todo el código en particular para las mismas funciones, por lo que
   pueden tener acceso a si mismas y tener asi recursión.

b) Explique por qué la gran mayoría de los lenguajes adoptan el scope estático por defecto.
R: El scope estático facilita el razonamiento de los programas, ya que el programador puede deducir los alcances
   de la variable que define, simplemente mirando el código. El scope dinámico rompe tal garantía, requiriendo
   que el programador anticipe todos los posibles contextos dinámicos.

c) Indique un escenario donde es útil tener scope dinámico.
R: Por ejemplo, si se quiere acceder a variables del sistema para adaptar un programa, el scope dinámico facilita
   las cosas, ya que la otra opción seria pasar tal variable función a función como parámetro, complicando el
   desarrollo. Algo similar también puede ser aplicado al manejo de excepciones.
|#

#| P2. ¿¿¿Primer que?
  <ExWAE> ::= <num>
  | (add <ExWAE> <ExWAE>)
  | (sub <ExWAE> <ExWAE>)
  | with (<id> <ExWAE>) <ExWAE>
  | <id>
  | (app <id> <ExWAE>)
  | (if0 <ExWAE> <ExWAE> <ExWAE>)

  <FunDef> ::= (fundef <id> <id> <expr>)


a) sum y fib
(fundef sum n (if0 n 0 (add n (app sum (sub n 1)))))
(fundef fib n
        (if0 n 1 (with (n-1 (sub n 1))
          (if0 n-1 1 (with (n-2 (sub n-1 1))
            (add (app fib n-1) (app fib n-2)))))))

b) even? y odd?
(fundef even? n
        (if0 n 0
          (if0 (app odd? (sub n 1)) 1 0)))
(fundef odd? n
        (if0 n 1
          (if0 (app even? (sub n 1)) 1 0)))
|#

#| P3. ¿Dinamico o Léxico?

a) (define my-funcs (list (fundef 'f 'y (parse '{+ y x}))))
     (run '{with {x 5}
             {+ {with {x 8} x}
               {f 3}}} my-funcs)

Con Lexic Scope este programa nos genera un error, ya que el alcance de la variable X no llega a la
definición de función.

Por otro lado, usando Dynamic Scope, el enlace de x a su valor 5, permanece dentro de la ejecución del
binding, por lo que el cuerpo de f sí puede acceder a su valor. Resultado: 16.

b) (define my-funcs (list (fundef 'f 'y (parse '{+ x n}))
                          (fundef 'g 'x (parse '{+ x {f x}}))))
     (run '{with {n 5} {g n}} my-funcs)

Similar al ejemplo de a), con Lexic Scope este programa nos genera un error, ya que el alcance de la
variable n no llega a la definición de función. Adicionalmente, el cuerpo de f también desconoce el valor
de x, por lo que tiene doble motivo para fallar.

Utilizando Dynamic Scope, el enlace de n a su valor 5, permanece dentro de la ejecución del binding, por
lo que el cuerpo de f sí puede acceder a su valor (aún habiendo pasado primero por g). Adicionalmente, g
introduce un enlace para X (argumento de g), el cual es visible por el cuerpo de f. Resultado: 15.
|#