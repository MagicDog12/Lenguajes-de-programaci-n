#lang play

;;;;
#|  COPIE Y PEGUE SU CODIGO DE LA PREGUNTA UNO   |#

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

(deftype Fundef
  (fundef name arg body))


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


;; lookUpNumV :: Expr -> numV-n
(define (lookUpNumV expr)
  (match expr
    [(numV n) n]
    [(boolV b) (error "Runtime type error: expected Number found Bool")]
    [(pairV lV rV) (error "Runtime type error: expected Number found Pair")]))

;; lookUpBoolV :: Expr -> boolV-b
(define (lookUpBoolV expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Bool found Number")]
    [(boolV b) b]
    [(pairV lV rV) (error "Runtime type error: expected Bool found Pair")]))

;; lookUpPairVfst :: Expr -> PairV-lV
(define (lookUpPairVfst expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Pair found Number")]
    [(boolV b) (error "Runtime type error: expected Pair found Bool")]
    [(pairV lV rV) lV]))

;; lookUpPairVsnd :: Expr -> PairV-rV
(define (lookUpPairVsnd expr)
  (match expr
    [(numV n) (error "Runtime type error: expected Pair found Number")]
    [(boolV b) (error "Runtime type error: expected Pair found Bool")]
    [(pairV lV rV) rV]))

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


#| LUEGO MODIFIQUELO SIGUIENDO LAS INSTRUCCIONES |#
;;;;

;;;; luego de copiar su codigo elimine las definiciones de Exp, Prog, parse y run
(deftype Exp
  (num n)
  (bool b))

(deftype Prog
  (prog funs main))
(define (parse p) (error "replace parse with your own implementation"))
;;;;

#|
<fundef> ::= {define {<id> {arg}*} [: <type>] <expr>}

<arg>    ::= {<id> : <type>}

<expr>   ::= ... | {with { {<id> [: <type>] <expr>}* } <expr>}  ; los otros casos no cambian

<type>   ::= Num | Bool | {Pair <type> <type>}
|#

(deftype Type
  (numT)
  (boolT)
  (pairT lT rT))

;; typecheck-expr :: ...
(define (typecheck-expr e)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    ; ...
    [_ (error "not yet implemented")]
    ))

;; typecheck-fundef :: ...
(define (typecheck-fundef f)
  ; ...
  (error "not yet implemented"))

;; typecheck :: ...
(define (typecheck p)
  (def (prog funs main) p)
  (begin
    (map typecheck-fundef funs)
    (typecheck-expr main)))

