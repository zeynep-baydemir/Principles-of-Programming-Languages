;zeynep baydemir
;2019400096
;compiling: yes
;complete: yes
#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))
; 10 points
(define -- (lambda args (list 'let args)))
; 10 points
(define @ (lambda (bindings expr) (append bindings expr)))
; 20 points
(define split_at_delim (lambda (delim args)
        (remove '()(foldr (lambda (elem after)
           (if (equal? elem delim)
               (cons '() after)
               (cons (cons elem (car after)) (cdr after))))
         (list '()) args))))
; 30 points
                 

(define parantezsiz(lambda (expr)
                      (car expr)))

(define parse_expr (lambda (expr)
                     (cond
                       [(member '+ expr)(cons '+ (map parse_expr (split_at_delim '+ expr)))]
                       [(member '* expr)(cons '* (map parse_expr (split_at_delim '* expr)))]
                       [(member '@ expr)(@ (car(map parse_expr (split_at_delim '@ expr)))(cdr(map parse_expr (split_at_delim '@ expr))))]
                       [(member '-- expr)(apply -- (map parse_expr (split_at_delim '-- expr)))]
                       [(member ':= expr)(apply := (map parse_expr (split_at_delim ':= expr)))]
                       [(list? (car expr)) (parse_expr(parantezsiz expr))]
                       [(eqv? (car expr) 'quote)(cadr expr)]
                       [else (car expr)])))
                       
                 
                     


; 20 points
(define eval_expr (lambda (expr)
                    (eval (parse_expr expr))))
