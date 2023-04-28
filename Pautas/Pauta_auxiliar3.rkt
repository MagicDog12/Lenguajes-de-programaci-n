#lang play

(print-only-errors #t)


; P1
#|
<Expr> ::= <number>
| {+ <Expr> <Expr>}
| {- <Expr> <Expr>}
| <symbol>
| {with {<symbol> <Expr>} <Expr>}

|#

(deftype Expr
  (num n)
  (binop op l r)
  (id x)
  (with name named-expr body))

(define (parse-binop bin)
  (match bin
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]))
(define binops (list '+ '- '* '/))
(define (is-binop? x) (member x binops))

; parse :: sexpr-v -> Expr
(define (parse sexpr)
  (match sexpr
    [(? number?) (num sexpr)]
    [(? symbol?) (id sexpr)]
    [(list (? is-binop? op) l r) (binop (parse-binop op) (parse l) (parse r))]
    [(list 'with (list i e) b)
     (with i (parse e) (parse b))]
    ))

;; subst : Id Val Expr -> Expr
; reemplaza las ocurrencias ???todas??? del Id por el Val en la Expr
(define (subst x val expr)
  (match expr
    [(num n) expr] ; no hay nada que hacer
    [(binop op l r) (op (subst x val l) (subst x val r))] ; propagar
    [(with y ne body)
     (with y ; no se toca (binding occurrence)
           (subst x val ne) ; siempre (named expr)
           (if (equal? x y)
               body ; no se toca, scope anidado!
               (subst x val body)))] ; propagar
    [(id y) (if (equal? x y)
                val    ; substituir!!!
                expr)] ; nada que hacer
    ))

; interp :: Expr -> number (o error)
(define (interp expr fundefs)
  (match expr
    [(num n) n]
    [(binop op l r) (op (interp l fundefs) (interp r fundefs))]
    [(id x) (error "free identifier:" x)]
    [(with x e b) (interp (subst x (num (interp e fundefs)) b) fundefs)]
    ))

; top-level run
(define (run prog)
  (interp (parse prog)))

; P2
; obfuscate :: Expr -> Expr
(define (obfuscate e)
  (match e
    [(num n) (num n)]
    [(binop op l r) (binop op (obfuscate l) (obfuscate r))]
    [(id x) (id x)]
    [(with bound-id named-expr body) (def new (gensym))
                                     (with new named-expr (obfuscate (subst bound-id (id new) body)))
                                     ]
    ))

; P3
(deftype Expr-d
  (num-d n)
  (binop-d op l r)
  (with x n e)
  (id x)
  (rel n)
  (with-d ne b))


;; subst : symbol x Expr-d x number -> Expr-d
(define (subst-d x expr val)
  (match expr

    [(binop op l r) (binop-d op
                           (subst-d x l val)
                           (subst-d x r val))]
    [(with bid ne b) (if (symbol=? bid x)
                         (with bid (subst x ne val) b)
                         ;else
                         (with bid (subst x ne val)
                               (subst x b (+ 1 val))))] ;; 1er modif.
    [(id v) (if (symbol=? v x) (rel val) expr)]       ;; 2da modif.
    [_ expr]))


;; toDeBruijn : Expr -> Expr-d
;; Transforma la expresion, eliminando los with clasico por los with con
;; indice De Bruijn
(define (toDeBruijn expr)
  (match expr
    [(num n) (num-d n)]
    [(binop op l r) (binop-d op (toDeBruijn l) (toDeBruijn r))]
    [(with bid ne b)
     (with-d (toDeBruijn ne)
	     (toDeBruijn (subst-d bid b 0)))]
    [(id v) (error 'free-identifier)]
    [else expr]))





