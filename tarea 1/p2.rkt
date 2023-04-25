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
(deftype Prog
  (prog fundefs main))

(deftype Fundef
  (fundef name arg tbody body))

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
(define (aux id type se)
  (list id type (parse-expr se)))

(define (type-check-aux se)
  (typecheck-expr (parse-expr se) empty-env empty-env))

;; parse-type :: s-Type -> Type
(define (parse-type st)
  (match st
    ['Num (numT)]
    ['Bool (boolT)]
    [(list 'Pair l r) (pairT (parse-type l) (parse-type r))]))

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
    [(list 'with (list (list name named-expr) ...) e) (def type (map type-check-aux named-expr))
                                                      (my-with (map aux name type named-expr) (parse-expr e))]
    [(list 'with (list (list name ': type named-expr) ...) e) (def new-type (map parse-type type))
                                                              (my-with (map aux name new-type named-expr) (parse-expr e))]
    [(list f e ...) (app f (map parse-expr e))]
    ))


;; parse-args :: s-args -> args
(define (parse-args sa st)
  (list sa (parse-type st)))

(define (make-env-args args env)
  (def arg (car args))
  (def type (car (cdr args)))
  (extend-env arg type env))

;; parse-fundef :: s-Fundef -> Fundef
(define (parse-fundef sf)
  (match sf
    [(list 'define (list name (list args ': types) ...) body) (def args-new (map parse-args args types))
                                              (def newEnv (foldl make-env-args empty-env args-new))
                                              (def tbody (typecheck-expr (parse-expr body) newEnv empty-env))
                                              (fundef name args-new tbody (parse-expr body))]
    [(list 'define (list name (list args ': types) ...) ': tbody body) (def args-new (map parse-args args types))
                                                       (def tbody-new (parse-type tbody))
                                                       (fundef name args-new tbody-new (parse-expr body))]))

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
     (def (fundef _ arg _ body) (lookup-fundef f funs))
     (interp body
             (auxEnv arg e env funs empty-env)
             funs)]
    ))

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

;; type-error :: type type -> #t / error
(define (type-error my-type t)
  (match my-type
    [(numT) (match t
              [(numT) #t]
              [(boolT) (error "Static type error: expected Num found Bool")]
              [(pairT lT rT) (error "Static type error: expected Num found Pair")])]
    [(boolT) (match t
              [(numT) (error "Static type error: expected Bool found Num")]
              [(boolT) #t]
              [(pairT lT rT) (error "Static type error: expected Bool found Pair")])]
    [(pairT lT rT) (match t
              [(numT) (error "Static type error: expected Pair found Num")]
              [(boolT) (error "Static type error: expected Pair found Bool")]
              [(pairT lT2 rT2) (def aux-lT (type-error lT lT2))
                             (type-error rT rT2)])]))

;; typecheck-expr :: expr -> type
(define (typecheck-expr e env funs)
  (match e
    [(num n) (numT)]
    [(bool b) (boolT)]
    [(id x) (env-lookup x env)]
    [(my-cons l r) (pairT (typecheck-expr l env funs) (typecheck-expr r env funs))]
    [(my-add1 e) (match (typecheck-expr e env funs)
                   [(numT) (numT)]
                   [(boolT) (error "Static type error: operator add1 expected Num found Bool")]
                   [(pairT lT rT) (error "Static type error: operator add1 expected Num found Pair")])]
    [(my-add l r) (match (typecheck-expr l env funs)
                    [(numT) (match (typecheck-expr r env funs)
                              [(numT) (numT)]
                              [(boolT) (error "Static type error: operator + expected Num found Bool")]
                              [(pairT lT rT) (error "Static type error: operator add1 expected Num found Pair")])]
                    [(boolT) (error "Static type error: operator + expected Num found Bool")]
                    [(pairT lT rT) (error "Static type error: operator add1 expected Num found Pair")])]
    [(my-< l r) (match (typecheck-expr l env funs)
                      [(numT) (match (typecheck-expr r env funs)
                                [(numT) (boolT)]
                                [(boolT) (error "Static type error: operator < expected Num found Bool")]
                                [(pairT lT rT) (error "Static type error: operator < expected Num found Pair")])]
                    [(boolT) (error "Static type error: operator < expected Num found Bool")]
                    [(pairT lT rT) (error "Static type error: operator < expected Num found Pair")])]
    [(my-= l r) (match (typecheck-expr l env funs)
                      [(numT) (match (typecheck-expr r env funs)
                                [(numT) (boolT)]
                                [(boolT) (error "Static type error: operator = expected Num found Bool")]
                                [(pairT lT rT) (error "Static type error: operator = expected Num found Pair")])]
                    [(boolT) (error "Static type error: operator = expected Num found Bool")]
                    [(pairT lT rT) (error "Static type error: operator = expected Num found Pair")])]
    [(my-! e) (match (typecheck-expr e env funs)
                   [(numT) (error "Static type error: operator ! expected Bool found Num")]
                   [(boolT) (boolT)]
                   [(pairT lT rT) (error "Static type error: operator ! expected Bool found Pair")])]
    [(my-and l r) (match (typecheck-expr l env funs)
                    [(numT) (error "Static type error: operator && expected Bool found Num")]
                    [(boolT) (match (typecheck-expr r env funs)
                                [(numT) (error "Static type error: operator && expected Bool found Num")]
                                [(boolT) (boolT)]
                                [(pairT lT rT) (error "Static type error: operator && expected Bool found Pair")])]
                    [(pairT lT rT) (error "Static type error: operator && expected Bool found Pair")])]
    [(my-or l r) (match (typecheck-expr l env funs)
                    [(numT) (error "Static type error: operator || expected Bool found Num")]
                    [(boolT) (match (typecheck-expr r env funs)
                                [(numT) (error "Static type error: operator || expected Bool found Num")]
                                [(boolT) (boolT)]
                                [(pairT lT rT) (error "Static type error: operator || expected Bool found Pair")])]
                    [(pairT lT rT) (error "Static type error: operator || expected Bool found Pair")])]
    [(fst e) (match (typecheck-expr e env funs)
               [(numT) (error "Static type error: operator fst expected Pair found Num")]
               [(boolT) (error "Static type error: operator fst expected Pair found Bool")]
               [(pairT lT rT) lT])]
    [(snd e) (match (typecheck-expr e env funs)
               [(numT) (error "Static type error: operator snd expected Pair found Num")]
               [(boolT) (error "Static type error: operator snd expected Pair found Bool")]
               [(pairT lT rT) rT])]
    [(my-if c t f) (match (typecheck-expr c env funs)
                     [(numT) (error "Static type error: expected Bool found Num")]
                     [(boolT) (match (typecheck-expr t env funs)
                                [(numT) (match (typecheck-expr f env funs)
                                          [(numT) (numT)]
                                          [(boolT) (error "Static type error: expected Num found Bool")]
                                          [(pairT lT rT) (error "Static type error: expected Num found Pair")])]
                                [(boolT) (match (typecheck-expr f env funs)
                                           [(numT) (error "Static type error: expected Bool found Num")]
                                           [(boolT) (boolT)]
                                           [(pairT lT rT) (error "Static type error: expected Bool found Pair")])]
                                [(pairT lT rT) (match (typecheck-expr f env funs)
                                                 [(numT) (error "Static type error: expected Pair found Num")]
                                                 [(boolT) (error "Static type error: expected Pair found Bool")]
                                                 [(pairT lT rT) (pairT lT rT)])])]
                     [(pairT lT rT) (error "Static type error: expected Bool found Pair")])]
    [(my-with list body) (cond
                           [(equal? list '()) (typecheck-expr body env funs)]
                           [else
                            (def id-declarado (car(car list)))
                            (def argumento-declarado (car (cdr (cdr (car list)))))
                            (def tipo-declarado (car (cdr (car list))))
                            (cond
                                   [(type-error tipo-declarado (typecheck-expr argumento-declarado env funs))
                                    (def newEnv (extend-env id-declarado tipo-declarado env))
                                    (typecheck-expr (my-with (cdr list) body) newEnv funs)]
                                   [else (error "te pasaste")])])]
    [(app name arg-expr) (def fun (lookup-fundef name funs))
                         (def (fundef _ arg tbody _) fun)
                         (cond
                           [(equal? arg-expr '()) fun]
                           [else (begin
                                   (check-args arg arg-expr env funs)
                                   tbody)])]))

(define (check-args arg arg-expr env funs)
  (cond
    [(equal? arg '()) (cond
                        [(equal? arg-expr '()) #t]
                        [else (error "Cantidad de argumentos dadas menor que las necesarias")])]
    [(equal? arg-expr '()) (error "Cantidad de argumentos dadas mayor que las necesarias")]
    [else (def t1 (car (cdr (car arg))))
          (def t2 (typecheck-expr (car arg-expr) env funs))
          (begin (type-error t1 t2)
                 (check-args (cdr arg) (cdr arg-expr) env funs))]))

;; typecheck-fundef :: fundef -> type
(define (typecheck-fundef f env)
  (def (fundef name arg tbody body) f)
  (def newEnv (foldl make-env-args env arg))
  (cond
     [(type-error tbody (typecheck-expr body newEnv empty-env)) newEnv]
     [else (error "pasó algo")]))


;; typecheck :: prog -> type
(define (typecheck p)
  (def (prog funs main) p)
  (def newEnv (foldl typecheck-fundef empty-env funs))
  (typecheck-expr main newEnv funs))