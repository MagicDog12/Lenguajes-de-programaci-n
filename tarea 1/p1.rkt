#lang play

(require "env.rkt")

;; Parte 1

#|
<prog>   ::= {<fundef>* <expr>}

<fundef> ::= {define {<id> <id>*} <expr>}

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
(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg body))

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

; busca funcion por nombre
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

;; parse :: s-Prog -> Prog
(define (parse sp)
  (match sp
    [(list ds ... e) (prog (map parse-fundef ds) (parse-expr e))] ;; ds es la lista de definiciones, e es la expresion principal
    ))

;; aux :: id s-Expr -> (my-cons id Expr)
(define (aux id se)
  (list id (parse-expr se)))

;; parse-expr :: s-Expr -> Expr
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


;; parse-fundef :: s-Fundef -> Fundef
(define (parse-fundef sf)
  (match sf
    [(list 'define (list name args ...) body) (fundef name args (parse-expr body))]))

;; auxEnv :: list list env funs -> env
(define (auxEnv args e envInterp funs envResult)
  (cond
    [(equal? args '()) envResult]
    [else (def extEnv (extend-env (car args) (interp (car e) envInterp funs) envResult))
          (auxEnv (cdr args) (cdr e) envInterp funs extEnv)]))


;; lookUpNumV :: Expr env funs -> numV-n
(define (lookUpNumV expr)
  (match expr
    [(numV n) n]
    [(boolV b) (error "Runtime type error: expected Number found Bool" expr)]
    [(pairV lV rV) (error "Runtime type error: expected Number found Pair" expr)]))

;; lookUpNumV :: Expr env funs -> numV-n
(define (lookUpNumV expr)
  (match expr
    [(numV n) n]
    [(boolV b) (error "Runtime type error: expected Number found Bool" expr)]
    [(pairV lV rV) (error "Runtime type error: expected Number found Pair" expr)]))

;; lookUpBoolV :: Expr env funs -> boolV-b
(define (lookUpBoolV expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Bool found Number" expr)]
    [(boolV b) b]
    [(pairV lV rV) (error "Runtime type error: expected Bool found Pair" expr)]))

;; interp :: Expr -> Env -> List[FunDef] -> Val
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
    [(fst e) (match (interp e env funs)
               [(pairV lV _) lV])]
    [(snd e) (match (interp e env funs)
               [(pairV _ rV) rV])]
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

(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

