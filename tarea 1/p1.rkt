#lang play

(require "env.rkt")

;; Parte 1

;; <prog>   ::= {<fundef>* <expr>}
(deftype Prog
  (prog fundefs main))

;; <fundef> ::= {define {<id> <id>*} <expr>}
(deftype Fundef
  (fundef name arg body))

#|
<expr>   ::= <num>
           | <id>
           | <bool>
           | {cons <expr> <expr>}
           | {add1 <expr>}
           | {+ <expr> <expr>}
           | {< <expr> <expr>}
           | {= <expr> <expr>}
           | {! <expr>}
           | {&& <expr> <expr>}
           | {|| <expr> <expr>}
           | {fst <expr>}
           | {snd <expr>}
           | {if <expr> <expr> <expr>}
           | {with {{<id> <expr>}*} <expr>}
           | {<id> <expr>*}
|#
(deftype Expr
  (num n)
  (id x)
  (bool b)
  (my-cons l r)
  (my-add1 e)
  (my-add l r)
  (my-< l r)
  (my-= l r)
  (my-! e)
  (my-and l r)
  (my-or l r)
  (fst e)
  (snd e)
  (my-if c t f)
  (my-with list body)
  (app name arg-expr)
  )
#|
lookup-fundef :: name list -> Fundef
Busca una funcion por nombre y retorna la funcion
|#
(define (lookup-fundef f funs)
  (match funs
    ['() (error "undefined function:" f)]
    [(cons fd rest)
     (if (equal? (fundef-name fd) f)
         fd
         (lookup-fundef f rest))]))

#|
tipo inductivo para los valores del lenguaje

representation BNF:
<val>   ::= {numV <num>}
           | {boolV <bool>}
           | {pairV <val> <val>}
|#
(deftype Val
  (numV n)
  (boolV b)
  (pairV lV rV))

#|
parse :: s-Prog -> Prog
Convierte un programa de sintaxis concreta a sintaxis abstracta
|#
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

#|
aux :: id s-Expr -> list
Toma un id y un se, y retorna una lista con id y se parseado
|#
(define (aux id se)
  (list id (parse-expr se)))

#|
parse-expr :: s-Expr -> Expr
Convierte una expresion de sintaxis concreta a sintaxis abstracta
|#
(define (parse-expr se)
  (match se
    [(? number?) (num se)]
    [(? symbol?) (id se)]
    [(? boolean?) (bool se)]
    [(list 'cons e1 e2) (my-cons (parse-expr e1) (parse-expr e2))]
    [(list 'add1 e) (my-add1 (parse-expr e))]
    [(list '+ e1 e2) (my-add (parse-expr e1) (parse-expr e2))]
    [(list '< e1 e2) (my-< (parse-expr e1) (parse-expr e2))]
    [(list '= e1 e2) (my-= (parse-expr e1) (parse-expr e2))]
    [(list '! e) (my-! (parse-expr e))]
    [(list '&& e1 e2) (my-and (parse-expr e1) (parse-expr e2))]
    [(list '|| e1 e2) (my-or (parse-expr e1) (parse-expr e2))]
    [(list 'fst e) (fst (parse-expr e))]
    [(list 'snd e) (snd (parse-expr e))]
    [(list 'if c t f) (my-if (parse-expr c) (parse-expr t) (parse-expr f))]
    [(list 'with (list (list name named-expr) ...) e) (my-with (map aux name named-expr) (parse-expr e))]
    [(list f e ...) (app f (map parse-expr e))]
    ))

#|
parse-fundef :: s-Fundef -> Fundef
Convierte una definicion de funcion de sintaxis concreta a sintaxis abstracta
y retorna una Fundef con su body parseado
|#
(define (parse-fundef sf)
  (match sf
    [(list 'define (list name args ...) body) (fundef name args (parse-expr body))]))

#|
auxEnv :: List[FunDef-arg] List[app-arg-expr] Env Funs -> Env
Toma una lista con argumentos proveniente de la definicion de la funcion,
toma una lista con los argumentos para evaluar la funcion, un ambiente y una lista de funciones y
devuele un ambiente extendido del ambiente dado donde se agregan estos argumentos al ambiente.
|#
(define (auxEnv args e envInterp funs envResult)
  (cond
    [(equal? args '()) envResult]
    [else (def extEnv (extend-env (car args) (interp (car e) envInterp funs) envResult))
          (auxEnv (cdr args) (cdr e) envInterp funs extEnv)]))

#|
lookUpNumV :: Expr -> numV-n
Recibe una expresion, le hace pattern matching y si es un numV retorna el valor,
de lo contrario da un Runtime type error
|#
(define (lookUpNumV expr)
  (match expr
    [(numV n) n]
    [(boolV b) (error "Runtime type error: expected Number found Bool")]
    [(pairV lV rV) (error "Runtime type error: expected Number found Pair")]))

#|
lookUpBoolV :: Expr -> boolV-b
Recibe una expresion, le hace pattern matching y si es un boolV retorna el valor,
de lo contrario da un Runtime type error
|#
(define (lookUpBoolV expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Bool found Number")]
    [(boolV b) b]
    [(pairV lV rV) (error "Runtime type error: expected Bool found Pair")]))

#|
lookUpPairVfst :: Expr -> PairV-lV
Recibe una expresion, le hace pattern matching y si es un pairV retorna el valor lV,
de lo contrario da un Runtime type error
|#
(define (lookUpPairVfst expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Pair found Number")]
    [(boolV b) (error "Runtime type error: expected Pair found Bool")]
    [(pairV lV rV) lV]))

#|
lookUpPairVsnd :: Expr -> PairV-rV
Recibe una expresion, le hace pattern matching y si es un pairV retorna el valor rV,
de lo contrario da un Runtime type error
|#
(define (lookUpPairVsnd expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Pair found Number")]
    [(boolV b) (error "Runtime type error: expected Pair found Bool")]
    [(pairV lV rV) rV]))

#|
interp :: Expr Env List[FunDef] -> Val
Recibe una expresion, un ambiente y una lista de funciones e interpreta la expresion en ese contexto
retornando un valor.
|#
(define (interp exp env funs)
  (match exp
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    [(my-cons l r) (pairV (interp l env funs) (interp r env funs))]
    [(my-add1 e) (numV (+ 1 (lookUpNumV (interp e env funs))))]
    [(my-add l r) (numV (+ (lookUpNumV (interp l env funs)) (lookUpNumV (interp r env funs))))]
    [(my-< l r) (boolV (< (lookUpNumV (interp l env funs)) (lookUpNumV (interp r env funs))))]
    [(my-= l r) (boolV (= (lookUpNumV (interp l env funs)) (lookUpNumV (interp r env funs))))]
    [(my-! e) (boolV (not (lookUpBoolV (interp e env funs))))]
    [(my-and l r) (boolV (and (lookUpBoolV (interp l env funs)) (lookUpBoolV (interp r env funs))))]
    [(my-or l r) (boolV (or (lookUpBoolV (interp l env funs)) (lookUpBoolV (interp r env funs))))]
    [(fst e) (lookUpPairVfst (interp e env funs))]
    [(snd e) (lookUpPairVsnd (interp e env funs))]
    [(my-if c t f) (if (lookUpBoolV (interp c env funs)) (interp t env funs) (interp f env funs))]
    [(my-with list body) (cond
                           [(equal? list '()) (interp body env funs)]
                           [else (interp (my-with (cdr list) body)
                                         (extend-env (car (car list)) (interp (car (cdr (car list))) env funs) env)
                                         funs)])]
    [(app f e)
     (def (fundef _ arg body) (lookup-fundef f funs))
     (interp body
             (auxEnv arg e env funs empty-env)
             funs)]
    ))

#|
run :: s-Prog -> Val
Recibe un programa en sintaxis concreta, lo parsea y luego lo interpreta retornando un valor.
|#
(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

