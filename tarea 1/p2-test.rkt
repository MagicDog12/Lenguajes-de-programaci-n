#lang play

(require "p2.rkt")
(print-only-errors #t)

;; Tests básicos:
;; Caso num
(test (typecheck (prog '() (num 5))) (numT))
;; Caso bool
(test (typecheck (prog '() (bool #t))) (boolT))
;; Caso cons
(test (typecheck (prog '() (my-cons (num 1) (num 2)))) (pairT (numT) (numT)))
(test (typecheck (prog '() (my-cons (bool #f) (bool #f)))) (pairT (boolT) (boolT)))
;; Caso add1
(test (typecheck (prog '() (my-add1 (num 1)))) (numT))
;; Caso +
(test (typecheck (prog '() (my-add (num 1) (num 2)))) (numT))
;; Caso <
(test (typecheck (prog '() (my-< (num 1) (num 2)))) (boolT))
;; Caso =
(test (typecheck (prog '() (my-= (num 1) (num 2)))) (boolT))
;; Caso !
(test (typecheck (prog '() (my-! (bool #f)))) (boolT))
;; Caso &&
(test (typecheck (prog '() (my-and (bool #f) (bool #t)))) (boolT))
;; Caso ||
(test (typecheck (prog '() (my-or (bool #f) (bool #t)))) (boolT))
;; Caso fst
(test (typecheck (prog '() (fst (my-cons (num 4) (num 6))))) (numT))
(test (typecheck (prog '() (fst (my-cons (bool #f) (bool #t))))) (boolT))
;; Caso snd
(test (typecheck (prog '() (snd (my-cons (num 4) (num 6))))) (numT))
(test (typecheck (prog '() (snd (my-cons (bool #f) (bool #t))))) (boolT))
;; Caso if
(test (typecheck (prog '() (my-if (bool #t) (num 1) (num 2)))) (numT))
(test (typecheck (prog '() (my-if (bool #f) (bool #t) (bool #f)))) (boolT))
(test (typecheck (prog '() (my-if (bool #t) (my-cons (num 1) (num 2)) (my-cons (bool #t) (bool #f))))) (pairT (boolT) (boolT)))
(test (typecheck (prog '() (my-if (bool #f) (my-cons (num 1) (num 2)) (my-cons (bool #t) (bool #f))))) (pairT (boolT) (boolT)))
;; Caso with (importante)
;; Caso sin argumentos
(test (typecheck (prog '() (my-with '() (my-add (num 1) (num 2))))) (numT))
;; Caso con un argumento no declarado
(test (typecheck (parse '{
                          {with {{x 1}}
                                {add1 x}}
                          }))
      (numT))
;; Caso con un argumento declarado
(test (typecheck (prog '() (my-with (list (list 'x (numT) (num 1))) (my-add1 (id 'x))))) (numT))
;; Caso con dos argumentos declarados
(test (typecheck (prog '() (my-with (list (list 'x (numT) (num 1)) (list 'y (numT) (num 2))) (my-add (id 'x) (id 'y))))) (numT))
;; Caso con dos argumentos no declarados
(test (typecheck (parse '{
                          {with {{x 1} {y 2}}
                                {+ x y}}
                          }))
      (numT))

;; Caso definicion de funcion
(test (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Num}} {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          }))
      (numT))
;; expresion del cuerpo tiene el mismo tipo que el tipo de retorno declarado?
(test (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Num}} : Num {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          }))
      (numT))
(test/exn (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Num}} : Bool {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          }))
      "Static type error: expected Bool found Num")
(test/exn (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Num}} : {Pair Num Num} {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          }))
      "Static type error: expected Pair found Num")
;; el tipo de retorno no se especifica
(test (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Num}} {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          }))
      (numT))
(test (typecheck (parse '{ 
                          {define {bool1 {x : Bool}} {! x}}
                          {bool1 #t}
                          }))
      (boolT))
(test (typecheck (parse '{ 
                          {define {pair1 {x : Num} {y : Num}} {cons {add1 x} {add1 y}}}
                          {pair1 5 10}
                          }))
      (pairT (numT) (numT)))
;; Caso en que el argumento tiene tipo Num y la funcion espera otro tipo
(test/exn (typecheck (parse (parse '{ 
                          {define {bool1 {x : Num}} {! x}}
                          {bool1 #t}
                          }))) "Static type error: operator ! expected Bool found Num")
(test/exn (typecheck (parse (parse '{ 
                          {define {pair1 {x : Num}} {fst x}}
                          {pair1 #t}
                          }))) "Static type error: operator fst expected Pair found Num")
;; Caso en que el argumento tiene tipo Bool y la funcion espera otro tipo
(test/exn (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : Bool}} {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          })) "Static type error: operator add1 expected Num found Bool")
(test/exn (typecheck (parse (parse '{ 
                          {define {pair1 {x : Bool}} {fst x}}
                          {pair1 #t}
                          }))) "Static type error: operator fst expected Pair found Bool")
;; Caso en que el argumento tiene tipo Pair y la funcion espera otro tipo
(test/exn (typecheck (parse (parse '{ 
                          {define {bool1 {x : {Pair Num Num}}} {! x}}
                          {bool1 #t}
                          }))) "Static type error: operator ! expected Bool found Pair")
(test/exn (typecheck (parse '{ 
                          {define {sum1 {x : Num} {y : {Pair Bool Bool}}} {+ {add1 x} {add1 y}}}
                          {sum1 5 10}
                          })) "Static type error: operator add1 expected Num found Pair")
;; Caso con operador add1
(test/exn (typecheck (parse '{{add1 #t}}))
          "Static type error: operator add1 expected Num found Bool")
(test/exn (typecheck (parse '{{add1 {cons 1 2}}}))
          "Static type error: operator add1 expected Num found Pair")
;; Caso con operador <
(test/exn (typecheck (parse '{{< 10 #t}}))
          "Static type error: operator < expected Num found Bool")
(test/exn (typecheck (parse '{{< 10 {cons 1 2}}}))
          "Static type error: operator < expected Num found Pair")
;; Caso con operador +
(test/exn (typecheck (parse '{{+ #t 10}}))
          "Static type error: operator + expected Num found Bool")
(test/exn (typecheck (parse '{{+ {cons 1 2} 10}}))
          "Static type error: operator + expected Num found Pair")
;; Caso con operador !
(test/exn (typecheck (parse '{{! 10}}))
          "Static type error: operator ! expected Bool found Num")
;; Caso con operador &&
(test/exn (typecheck (parse '{{&& 10 #t}}))
          "Static type error: operator && expected Bool found Num")
(test/exn (typecheck (parse '{{&& #t {cons 1 2}}}))
          "Static type error: operator && expected Bool found Pair")
(test/exn (typecheck (parse '{{&& 20 10}}))
          "Static type error: operator && expected Bool found Num")
;; Caso con operador ||
(test/exn (typecheck (parse '{{|| 10 #t}}))
          "Static type error: operator || expected Bool found Num")
(test/exn (typecheck (parse '{{|| #t {cons 1 2}}}))
          "Static type error: operator || expected Bool found Pair")
(test/exn (typecheck (parse '{{|| 20 10}}))
          "Static type error: operator || expected Bool found Num")
;; Caso con operador fst
(test/exn (typecheck (parse '{{fst 10}}))
          "Static type error: operator fst expected Pair found Num")
(test/exn (typecheck (parse '{{fst #t}}))
          "Static type error: operator fst expected Pair found Bool")
;; Caso con operador snd
(test/exn (typecheck (parse '{{snd 10}}))
          "Static type error: operator snd expected Pair found Num")
(test/exn (typecheck (parse '{{snd #t}}))
          "Static type error: operator snd expected Pair found Bool")
;; = solo compara numeros
(test (typecheck (prog '() (my-= (num 4) (num 2)))) (boolT))
(test/exn (typecheck (prog '() (my-= (bool #t) (num 2)))) "Static type error: operator = expected Num found Bool")
(test/exn (typecheck (prog '() (my-= (num 1) (bool #f)))) "Static type error: operator = expected Num found Bool")
(test/exn (typecheck (prog '() (my-= (my-cons (num 1) (num 2)) (num 2)))) "Static type error: operator = expected Num found Pair")
(test/exn (typecheck (prog '() (my-= (num 1) (my-cons (num 1) (num 2))))) "Static type error: operator = expected Num found Pair")
;; expresion if condicion es de tipo bool
(test (typecheck (prog '() (my-if (bool #t) (num 1) (num 2)))) (numT))
(test/exn (typecheck (prog '() (my-if (num 0) (num 1) (num 2)))) "Static type error: expected Bool found Num")
(test/exn (typecheck (prog '() (my-if (my-cons (num 0) (num 1)) (num 1) (num 2)))) "Static type error: expected Bool found Pair")
;; expresion if ambas ramas deben tener el mismo tipo
(test/exn (typecheck (prog '() (my-if (bool #t) (num 1) (bool #t)))) "Static type error: expected Num found Bool")
(test/exn (typecheck (prog '() (my-if (bool #t) (bool #t) (num 1)))) "Static type error: expected Bool found Num")
(test/exn (typecheck (prog '() (my-if (bool #t) (my-cons (bool #t) (num 1)) (bool #t)))) "Static type error: expected Pair found Bool")
(test/exn (typecheck (prog '() (my-if (bool #t) (bool #t) (my-cons (bool #t) (num 1))))) "Static type error: expected Bool found Pair")
(test/exn (typecheck (prog '() (my-if (bool #t) (my-cons (bool #t) (num 1)) (num 1)))) "Static type error: expected Pair found Num")
(test/exn (typecheck (prog '() (my-if (bool #t) (num 1) (my-cons (bool #t) (num 1))))) "Static type error: expected Num found Pair")
;; tipo resultante de la expresion if es el tipo de las ramas
(test (typecheck (prog '() (my-if (bool #t) (num 1) (num 2)))) (numT))
(test (typecheck (prog '() (my-if (bool #t) (bool #t) (bool #f)))) (boolT))
(test (typecheck (prog '() (my-if (bool #t) (my-cons (bool #t) (num 1)) (my-cons (bool #t) (num 1))))) (pairT (boolT) (numT)))
;; En la app el numero de argumentos coincide
(test (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          })) (pairT (numT) (numT)))
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1}}
                          }))
      "Static type error: more arguments were expected" )
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 2 3}
                          }))
      "Static type error: fewer arguments were expected" )
;; En la app los tipos de los argumentos coinciden con los tipos esperados
(test  (typecheck (parse '{{define {f {x : Num}} {+ x 2}}
                     {f 1}}))
        (numT))
(test/exn  (typecheck (parse '{{define {f {x : Num}} {+ x 2}}
                     {f #t}}))
        "Static type error: expected Num found Bool")
(test/exn  (typecheck (parse '{{define {f {x : Num}} {+ x 2}}
                     {f {cons 1 2}}}))
        "Static type error: expected Num found Pair")
(test  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f #t}}))
        (numT))
(test/exn  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f 1}}))
        "Static type error: expected Bool found Num")
(test/exn  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f {cons 1 2}}}))
        "Static type error: expected Bool found Pair")
(test  (typecheck (parse '{{define {f {x : {Pair Num Num}}} {fst x}}
                     {f {cons 2 3}}}))
        (numT))
(test/exn  (typecheck (parse '{{define {f {x : {Pair Num Num}}} {fst x}}
                     {f 2}}))
        "Static type error: expected Pair found Num")
(test/exn  (typecheck (parse '{{define {f {x : {Pair Num Num}}} {fst x}}
                     {f #f}}))
           "Static type error: expected Pair found Bool")
(test/exn  (typecheck (parse '{{define {f {x : {Pair Num Num}}} {fst x}}
                     {f {cons 1 #f}}}))
           "Static type error: expected Num found Bool")
(test/exn  (typecheck (parse '{{define {f {x : {Pair Num Num}}} {fst x}}
                     {f {cons {cons 3 4} #f}}}))
           "Static type error: expected Num found Pair")
;; El tipo resultante de una app es el tipo de retorno de la funcion
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : Num
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          })) "Static type error: expected Num found Pair")
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : Bool
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          })) "Static type error: expected Bool found Pair")
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Bool}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          })) "Static type error: expected Bool found Num")
(test/exn (typecheck (parse '{ 
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair {Pair Num Num} Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          })) "Static type error: expected Pair found Num")
;; Errores de identificadores libres
(test/exn (run '{
                 {with {{x : Num 5} {y : Num 10}}
                       {+ x z}}})
          "free identifier: z")
(test/exn (run '{
                 {define {f {x : Num}} {+ x 3}}
                 {f x}})
          "free identifier: x")
;; Errores de funciones no definidas
(test/exn (run '{
                 {define {f {x : Num}} {+ x 3}}
                 {g 3}})
          "undefined function: 'g")

(test (typecheck (parse '{ ; Programa de ejemplo 1
                          {with {{x : Num 5} {y : Num 10}}
                                {+ x y}}
                          }))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 2
                          {with {{x 5}}
                                {with {{y : Num {+ x 1}}}
                                      {+ x y}}
                                }}))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 3
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          }))
      (pairT (numT) (numT)))

(test (typecheck (parse '{ ; Programa de ejemplo 4
                          {define {id {x : Num}} x}
                          {id 5}
                          }))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 5
                          {define {sum {x : Num} {y : Num} {z : Num}}
                            {+ x {+ y z}}}
                          {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
                            {fst {snd x}}}
                          {with {{x 9} {y {cons 1 {cons 3 4}}}}
                                {sum x {fst y} {cadr y}} }
                          }))
      (numT))

(test (typecheck (parse '{ ; Programa de ejemplo 6
                          {define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                            {cons {+ {fst p} x} {+ {snd p} x}}}
                          {add-pair {cons 1 1} 1}
                          }))
      (pairT (numT) (numT)))

(test (typecheck (parse '{3})) (numT))

(test  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f {< 3 4}}}))
        (numT))

(test/exn (typecheck (parse '{
                   {define {id {x : Num}} : Num {< x 3}}
                   {id #t}})) "Static type error: expected Num found Bool")

(test (typecheck (parse '{
                   {define {id {x : Num}} x}
                   {id 5}})) (numT))

(test (typecheck (parse '{
                   {define {id {x : Num} {y : Num}} {+ x y}}
                   {id 5 3}})) (numT))

(test (typecheck (parse '{
                   {define {id {x : Num}} : Num x}
                   {id 5}})) (numT))

(test/exn (typecheck (parse '{
                   {define {id {x : Num} {y : Num}} : Bool {+ x y}}
                   {id 5 3}})) "Static type error: expected Bool found Num")

(test/exn (typecheck (parse '{{define {one {x : Num}} 1}
                       {one #t}}))
          "Static type error: expected Num found Bool")

(test/exn (typecheck (parse '{{< 10 #t}}))
          "Static type error: operator < expected Num found Bool")

(test/exn (typecheck (parse '{{if 73 #t #t}}))
          "Static type error: expected Bool found Num")

(test/exn (typecheck (parse '{{with {{x : Num 5} {y : Num #t} {z : Num 42}}
                             z}}))
          "Static type error: expected Num found Bool")

;; Todos los casos pero con run:
(test (run '{5}) (numV 5))
(test (run '{#t}) (boolV #t))
(test (run '{{cons 1 2}}) (pairV (numV 1) (numV 2)))
(test (run '{{cons #f #f}}) (pairV (boolV #f) (boolV #f)))
(test (run '{{add1 1}}) (numV 2))
(test (run '{{+ 1 2}}) (numV 3))
(test (run '{{< 1 2}}) (boolV #t))
(test (run '{{= 1 2}}) (boolV #f))
(test (run '{{! #f}}) (boolV #t))
(test (run '{{&& #f #t}}) (boolV #f))
(test (run '{{|| #f #t}}) (boolV #t))
(test (run '{{fst {cons 4 6}}}) (numV 4))
(test (run '{{fst {cons #f #t}}}) (boolV #f))
(test (run '{{snd {cons 4 6}}}) (numV 6))
(test (run '{{snd {cons #f #t}}}) (boolV #t))
(test (run '{{if #t 1 2}}) (numV 1))
(test (run '{{if #f #t #f}}) (boolV #f))
(test (run '{{if #t {cons 1 2} {cons #t #f}}}) (pairV (numV 1) (numV 2)))
(test (run '{{if #f {cons 1 2} {cons #t #f}}}) (pairV (boolV #t) (boolV #f)))
(test (run '{{with {} {+ 1 2}}}) (numV 3))
(test (run '{{with {{x 1}} {add1 x}}}) (numV 2))
(test (run '{{with {{x : Num 1}}  {add1 x}}}) (numV 2))
(test (run '{{with {{x : Num 1} {y  : Num 2}} {+ x y}}}) (numV 3))
(test (run '{{with {{x 1} {y 2}} {+ x y}}}) (numV 3))
(test (run '{{define {sum1 {x : Num} {y : Num}} {+ {add1 x} {add1 y}}}
             {sum1 5 10}}) (numV 17))
(test (run '{{define {sum1 {x : Num} {y : Num}} : Num {+ {add1 x} {add1 y}}}
             {sum1 5 10}}) (numV 17))
(test/exn (run '{{define {sum1 {x : Num} {y : Num}} : Bool {+ {add1 x} {add1 y}}}
                 {sum1 5 10}}) "Static type error: expected Bool found Num")
(test/exn (run '{{define {sum1 {x : Num} {y : Num}} : {Pair Num Num} {+ {add1 x} {add1 y}}}
                 {sum1 5 10}}) "Static type error: expected Pair found Num")
(test (run '{{define {sum1 {x : Num} {y : Num}} {+ {add1 x} {add1 y}}}
             {sum1 5 10}}) (numV 17))
(test (run '{{define {bool1 {x : Bool}} {! x}}
             {bool1 #t}}) (boolV #f))
(test (run '{{define {pair1 {x : Num} {y : Num}} {cons {add1 x} {add1 y}}}
             {pair1 5 10}}) (pairV (numV 6) (numV 11)))
(test/exn (run '{{define {bool1 {x : Num}} {! x}}
                 {bool1 #t}}) "Static type error: operator ! expected Bool found Num")
(test/exn (run '{{define {pair1 {x : Num}} {fst x}}
                 {pair1 #t}}) "Static type error: operator fst expected Pair found Num")
(test/exn (run '{{define {sum1 {x : Num} {y : Bool}} {+ {add1 x} {add1 y}}}
                 {sum1 5 10}}) "Static type error: operator add1 expected Num found Bool")
(test/exn (run '{{define {pair1 {x : Bool}} {fst x}}
                 {pair1 #t}}) "Static type error: operator fst expected Pair found Bool")
(test/exn (run '{{define {bool1 {x : {Pair Num Num}}} {! x}}
                 {bool1 #t}}) "Static type error: operator ! expected Bool found Pair")
(test/exn (run '{{define {sum1 {x : Num} {y : {Pair Bool Bool}}} {+ {add1 x} {add1 y}}}
                 {sum1 5 10}}) "Static type error: operator add1 expected Num found Pair")
(test/exn (run '{{add1 #t}}) "Static type error: operator add1 expected Num found Bool")
(test/exn (run '{{add1 {cons 1 2}}}) "Static type error: operator add1 expected Num found Pair")
(test/exn (run '{{< 10 #t}}) "Static type error: operator < expected Num found Bool")
(test/exn (run '{{< 10 {cons 1 2}}}) "Static type error: operator < expected Num found Pair")
(test/exn (run '{{+ #t 10}}) "Static type error: operator + expected Num found Bool")
(test/exn (run '{{+ {cons 1 2} 10}}) "Static type error: operator + expected Num found Pair")
(test/exn (run '{{! 10}}) "Static type error: operator ! expected Bool found Num")
(test/exn (run '{{&& 10 #t}}) "Static type error: operator && expected Bool found Num")
(test/exn (run '{{&& #t {cons 1 2}}}) "Static type error: operator && expected Bool found Pair")
(test/exn (run '{{&& 20 10}}) "Static type error: operator && expected Bool found Num")
(test/exn (run '{{|| 10 #t}}) "Static type error: operator || expected Bool found Num")
(test/exn (run '{{|| #t {cons 1 2}}}) "Static type error: operator || expected Bool found Pair")
(test/exn (run '{{|| 20 10}}) "Static type error: operator || expected Bool found Num")
(test/exn (run '{{fst 10}}) "Static type error: operator fst expected Pair found Num")
(test/exn (run '{{fst #t}}) "Static type error: operator fst expected Pair found Bool")
(test/exn (run '{{snd 10}}) "Static type error: operator snd expected Pair found Num")
(test/exn (run '{{snd #t}}) "Static type error: operator snd expected Pair found Bool")
(test (run '{{= 4 2}}) (boolV #f))
(test/exn (run '{{= #t 2}}) "Static type error: operator = expected Num found Bool")
(test/exn (run '{{= 1 #f}}) "Static type error: operator = expected Num found Bool")
(test/exn (run '{{= {cons 1 2} 2}}) "Static type error: operator = expected Num found Pair")
(test/exn (run '{{= 1 {cons 1 2}}}) "Static type error: operator = expected Num found Pair")
(test (run '{{if #t 1 2}}) (numV 1))
(test/exn (run '{{if 0 1 2}}) "Static type error: expected Bool found Num")
(test/exn (run '{{if {cons 0 1} 1 2}}) "Static type error: expected Bool found Pair")
(test/exn (run '{{if #t 1 #t}}) "Static type error: expected Num found Bool")
(test/exn (run '{{if #t #t 1}}) "Static type error: expected Bool found Num")
(test/exn (run '{{if #t {cons #t 1} #t}}) "Static type error: expected Pair found Bool")
(test/exn (run '{{if #t #t {cons #t 1}}}) "Static type error: expected Bool found Pair")
(test/exn (run '{{if #t {cons #t 1} 1}}) "Static type error: expected Pair found Num")
(test/exn (run '{{if #t 1 {cons #t 1}}}) "Static type error: expected Num found Pair")
(test (run '{{if #t 1 2}}) (numV 1))
(test (run '{{if #t #t #f}}) (boolV #t))
(test (run '{{if #t {cons #t 1} {cons #t 1}}}) (pairV (boolV #t) (numV 1)))
(test (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
               {cons {+ {fst p} x} {+ {snd p} x}}}
             {add-pair {cons 1 1} 1}}) (pairV (numV 2) (numV 2)))
(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1}}}) "Static type error: more arguments were expected" )
(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1} 2 3}}) "Static type error: fewer arguments were expected" )
(test  (run '{{define {f {x : Num}} {+ x 2}}
              {f 1}}) (numV 3))
(test/exn  (run '{{define {f {x : Num}} {+ x 2}}
                  {f #t}}) "Static type error: expected Num found Bool")
(test/exn  (run '{{define {f {x : Num}} {+ x 2}}
                  {f {cons 1 2}}}) "Static type error: expected Num found Pair")
(test  (run '{{define {f {p : Bool}} {if p 23 42}}
              {f #t}}) (numV 23))
(test/exn  (run '{{define {f {p : Bool}} {if p 23 42}}
                  {f 1}}) "Static type error: expected Bool found Num")
(test/exn  (run '{{define {f {p : Bool}} {if p 23 42}}
                  {f {cons 1 2}}}) "Static type error: expected Bool found Pair")
(test  (run '{{define {f {x : {Pair Num Num}}} {fst x}}
              {f {cons 2 3}}}) (numV 2))
(test/exn  (run '{{define {f {x : {Pair Num Num}}} {fst x}}
                  {f 2}}) "Static type error: expected Pair found Num")
(test/exn  (run '{{define {f {x : {Pair Num Num}}} {fst x}}
                  {f #f}}) "Static type error: expected Pair found Bool")
(test/exn  (run '{{define {f {x : {Pair Num Num}}} {fst x}}
                  {f {cons 1 #f}}}) "Static type error: expected Num found Bool")
(test/exn  (run '{{define {f {x : {Pair Num Num}}} {fst x}}
                  {f {cons {cons 3 4} #f}}}) "Static type error: expected Num found Pair")

(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : Num
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1} 1}}) "Static type error: expected Num found Pair")
(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : Bool
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1} 1}}) "Static type error: expected Bool found Pair")
(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Bool}
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1} 1}}) "Static type error: expected Bool found Num")
(test/exn (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair {Pair Num Num} Num}
                   {cons {+ {fst p} x} {+ {snd p} x}}}
                 {add-pair {cons 1 1} 1}}) "Static type error: expected Pair found Num")
(test/exn (run '{{with {{x : Num 5} {y : Num 10}}
                       {+ x z}}}) "free identifier: z")
(test/exn (run '{{define {f {x : Num}} {+ x 3}}
                 {f x}}) "free identifier: x")
(test/exn (run '{{define {f {x : Num}} {+ x 3}}
                 {g 3}}) "undefined function: 'g")
(test (run '{{with {{x : Num 5} {y : Num 10}}
                   {+ x y}}}) (numV 15))
(test (run '{{with {{x 5}}
                   {with {{y : Num {+ x 1}}}
                         {+ x y}}}}) (numV 11))
(test (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
               {cons {+ {fst p} x} {+ {snd p} x}}}
             {add-pair {cons 1 1} 1}}) (pairV (numV 2) (numV 2)))
(test (run '{{define {id {x : Num}} x}
             {id 5}}) (numV 5))
(test (run '{{define {sum {x : Num} {y : Num} {z : Num}}
               {+ x {+ y z}}}
             {define {cadr {x : {Pair Num {Pair Num Num}}}} : Num
               {fst {snd x}}}
             {with {{x 9} {y {cons 1 {cons 3 4}}}}
                   {sum x {fst y} {cadr y}}}}) (numV 13))
(test (run '{{define {add-pair {p : {Pair Num Num}} {x : Num}} : {Pair Num Num}
               {cons {+ {fst p} x} {+ {snd p} x}}}
             {add-pair {cons 1 1} 1}}) (pairV (numV 2) (numV 2)))
(test (run '{3}) (numV 3))
(test  (run '{{define {f {p : Bool}} {if p 23 42}}
              {f {< 3 4}}}) (numV 23))
(test/exn (run '{{define {id {x : Num}} : Num {< x 3}}
                 {id #t}}) "Static type error: expected Num found Bool")
(test (run '{{define {id {x : Num}} x}
             {id 5}}) (numV 5))
(test (run '{{define {id {x : Num} {y : Num}} {+ x y}}
             {id 5 3}}) (numV 8))
(test (run '{{define {id {x : Num}} : Num x}
             {id 5}}) (numV 5))
(test/exn (run '{{define {id {x : Num} {y : Num}} : Bool {+ x y}}
                 {id 5 3}}) "Static type error: expected Bool found Num")

(test/exn (run '{{define {one {x : Num}} 1}
                 {one #t}}) "Static type error: expected Num found Bool")

(test/exn (run '{{< 10 #t}}) "Static type error: operator < expected Num found Bool")

(test/exn (run '{{if 73 #t #t}}) "Static type error: expected Bool found Num")

(test/exn (run '{{with {{x : Num 5} {y : Num #t} {z : Num 42}}
                       z}}) "Static type error: expected Num found Bool")
