#lang typed/racket

(require typed/rackunit)

#|
  LC = num
     | id
     | (/ id => LC)
     | (LC LC)
     | (+ LC LC)
     | (* LC LC)
     | (ifleq0 LC LC LC)
     | (println LC)
|#


; DEFINITION - Abstract syntax of WTUQ takes form of ExprC's
(define-type ExprC (U NumC IdC IfleqC LamC AppC PlusC MultC PrintC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([s : Symbol]) #:transparent)
(struct IfleqC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct LamC ([params : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [args : (Listof ExprC)]) #:transparent)


; DEFINITION - top-translate takes in an sexpr in LC form and translates it to JS
(define (top-translate [func-sexps : Sexp]) : String
  (interp (parse func-sexps)))


; DEFINITION - translate, takes in exprc and converts it to string form of js
(define (translate [exp : ExprC]) : String
  (match exp
    ))


; DEFINITION - parse
; parse takes in an s-expression and returns an expression in ExprC format
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? string? s) (StringC s)]
    [(? symbol? s) (cond
                     [(not-reserved? s) (IdC s)]
                     [else (error 'parse "WTUQ: Reserved identifier used ~e" s)])]
    [(list 'if opts ...) (cond
                           [(= 3 (length opts))
                            (IfC (parse (first opts)) (parse (second opts)) (parse (third opts)))]
                           [else
                            (error 'if "WTUQ: If requires test, true and false conditions ~e" opts)])]
    [(list 'lam (list (? symbol? params) ...) body)
     (LamC (validate-unique-params (cast params (Listof Symbol))) (parse body))]
    [(list 'local (list lams ...) 'in body)
     (desugar-local lams (parse body))]
    [(list expressions ...)
     ; in form of {function args ...}
     (AppC (parse (first expressions)) (map (lambda ([expr : Sexp]) (parse expr)) (rest expressions)))]))



