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
           | {! <expr> <expr>}
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
  (my-! l r)
  (my-and l r)
  (my-or l r)
  (fst e)
  (snd e)
  (my-if c t f)
  (my-with list body)
  (app name arg-expr)
  )

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
    [(list 'with l e) (my-with (map (cons (car l) (parse-expr (cadr l))) l) (parse-expr e))]
    [(list f e) (app f (parse-expr e))]
    ))


;; parse-fundef :: s-Fundef -> Fundef
(define (parse-fundef sf)
  ; ...
  (error "not yet implemented"))


;; interp :: Expr -> Env -> List[FunDef] -> Val
(define (interp e env funs)
  (match e
    [(num n) (numV n)]
    [(id x) (env-lookup x env)]
    [(bool b) (boolV b)]
    ; ...
    [_ (error "not yet implemented")]
    ))

(define (run sp)
  (def (prog funs main) (parse sp))
  (interp main empty-env funs))

