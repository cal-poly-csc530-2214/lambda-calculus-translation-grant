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
(struct LamC ([param : Symbol] [body : ExprC]) #:transparent)
(struct AppC ([func : ExprC] [arg : ExprC]) #:transparent)
(struct PlusC ([l : ExprC] [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC] [r : ExprC]) #:transparent)
(struct PrintC ([exp : ExprC]) #:transparent)


; DEFINITION - top-translate takes in an sexpr in LC form and translates it to JS
(define (top-translate [func-sexps : Sexp]) : String
  (translate (parse func-sexps)))


; DEFINITION - translate, takes in exprc and converts it to string form of js
(define (translate [exp : ExprC]) : String
  (match exp
    [(NumC n) (~v n)]
    [(IdC s) (symbol->string s)]
    [(LamC param body) (string-append "(" (symbol->string param) " => " (translate body) ")")]
    [(PlusC l r) (string-append "(" (translate l) " + " (translate r) ")")]
    [(MultC l r) (string-append "(" (translate l) " * " (translate r) ")")]
    [(IfleqC test t f) (string-append "if (" (translate test) " <= 0) { " (translate t) " } else { " (translate f) " }")]
    [(PrintC expr) (string-append "console.log(" (translate expr) ");")]
    [(AppC funcname body) (string-append "(" (translate funcname) "(" (translate body) "))")]))


; DEFINITION - parse
; parse takes in an s-expression and returns an expression in ExprC format
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? s) (IdC s)]
    [(list '/ (? symbol? param) '=> body) (LamC param (parse body))]
    [(list '+ l r) (PlusC (parse l) (parse r))]
    [(list '* l r) (MultC (parse l) (parse r))]
    [(list 'ifleq0 test tru fals) (IfleqC (parse test) (parse tru) (parse fals))]
    [(list 'println exp) (PrintC (parse exp))]
    [(list funcname arg) (AppC (parse funcname) (parse arg))]))



(check-equal? (parse '5) (NumC 5))
(check-equal? (parse 'x) (IdC 'x))
(check-equal? (parse '(/ x => (/ y => (+ x y)))) (LamC 'x (LamC 'y (PlusC (IdC 'x) (IdC 'y)))))
(check-equal? (parse '(f x)) (AppC (IdC 'f) (IdC 'x)))
(check-equal? (parse '(* y x)) (MultC (IdC 'y) (IdC 'x)))
(check-equal? (parse '(ifleq0 (+ x y) y x)) (IfleqC (PlusC (IdC 'x) (IdC 'y)) (IdC 'y) (IdC 'x)))
(check-equal? (parse '(println x)) (PrintC (IdC 'x)))



(check-equal? (top-translate '(+ x y)) "(x + y)")
(check-equal? (top-translate '(+ (+ x y) z)) "((x + y) + z)")
(check-equal? (top-translate '(+ (* x y) z)) "((x * y) + z)")
(check-equal? (top-translate '(+ (* x 5) z)) "((x * 5) + z)")
(check-equal? (top-translate '(/ x => (+ x 5))) "(x => (x + 5))")
(check-equal? (top-translate '(/ x => (ifleq0 x x (+ x 5)))) "(x => if (x <= 0) { x } else { (x + 5) })")
(check-equal? (top-translate '(println z)) "console.log(z);")
(check-equal? (top-translate '((/ x => (ifleq0 x x (+ x 5))) 3)) "((x => if (x <= 0) { x } else { (x + 5) })(3))")



