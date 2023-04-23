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
(test (typecheck (prog '() (fst (my-cons (num 4) (num 6))))) (numT))
(test (typecheck (prog '() (my-if (my-cons (bool #f) (bool #t))))) (boolT))
;; Caso with (importante)
;; Caso app


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

(test (typecheck (parse '{3})) (numT))

(test  (typecheck (parse '{{define {f {p : Bool}} {if p 23 42}}
                     {f {< 3 4}}}))
        (numT))

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


