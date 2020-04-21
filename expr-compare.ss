#lang racket
(provide expr-compare)

(define (lambda? x)
  (member x '(lambda λ)))

(define (two-num x y)
  (cond ((= x y) x)
        (else `(if % ,x ,y))))

(define (two-boolean x y)
  (cond ((equal? x y) x)
        (x '%)
        (else '(not %))))

(define (lambda-handler x y)
  (cond ((not (= (length (cadr x)) (length (cadr y)))) `(if % ,x ,y))
        ((and (equal? (car x) 'lambda) (equal? (car y) 'lambda))
         (cons 'lambda (lambda-check (cdr x) (cdr y))))
        (else (cons 'λ (lambda-check (cdr x) (cdr y))))))

(define (bind-single-var x y)
  (if (equal? x y)
      x
      (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))))

(define (bind-var x y)
  (if (and (equal? x '()) (equal? y '()))
      '()
      (cons (bind-single-var (car x) (car y)) (bind-var (cdr x) (cdr y)))))

(define (lambda-helper x parax y paray bind num)
  (cond ( (= num 1)
  (cond ((equal? x '()) '())
        ((and (list? (car x)) (not (lambda? (caar x)))) (cons (lambda-helper (car x) parax (car y) paray bind num) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        ((and (list? (car x)) (lambda? (caar x))) (cons (lambda-handler (car x) (car y)) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        ((not (member (car x) parax)) (cons (car x) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        (else (let ((ind (index-of parax (car x)))) (cons (list-ref bind ind) (lambda-helper (cdr x) parax (cdr y) paray bind num))))))
     
      (else (cond ((equal? y '()) '())
        ((and (list? (car y)) (not (lambda? (caar y)))) (cons (lambda-helper (car x) parax (car y) paray bind num) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        ((and (list? (car y)) (lambda? (caar y))) (cons (lambda-handler (car x) (car y)) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        ((not (member (car y) paray)) (cons (car y) (lambda-helper (cdr x) parax (cdr y) paray bind num)))
        (else (let ((ind (index-of paray (car y)))) (cons (list-ref bind ind) (lambda-helper (cdr x) parax (cdr y) paray bind num))))))))
    

(define (lambda-check x y)
  (let ((binding (bind-var (car x) (car y))))
                (cond
                  ((xor (list? (cadr x)) (list? (cadr y))) (list binding `(if % ,(cadr x) ,(cadr y))))
                  ((and (and (list? (cadr x)) (list? (cadr y)) (not (= (length (cadr x)) (length (cadr y))))))  (list binding `(if % ,(cadr x) ,(cadr y))))
                  (else
               (let ((xb (lambda-helper (cdr x) (car x) (cdr y) (car y) binding 1))(yb (lambda-helper (cdr x) (car x) (cdr y) (car y) binding 2)))
                 (cons binding (check-list xb yb)))))))


(define (check-single-term x y)
  (cond ((and (boolean? x) (boolean? y)) (two-boolean x y))
        ((equal? x y) x)
        (else `(if % ,x ,y))))

(define (check-list x y)
  (let check ((x x)(y y))
    (cond ((equal? x '()) '())
          ((equal? y '()) '())
          
          ;;; in case we find special clauses midway
          ((or (or (list? (car x)) (list? (car y)))
               ;(or (lambda? (car x)) (lambda? (car y)))
               ;(or (equal? (car x) 'if) (equal? (car y) 'if))
               ;(or (equal? (car x) 'quote) (equal? (car y) 'quote)
               )
           (cons (check-clause (car x) (car y)) (check (cdr x) (cdr y))))
          (else (cons (check-single-term (car x) (car y)) (check (cdr x) (cdr y)))))))


(define (check-clause x y)
  (cond ((and (empty? x) (empty? y)) '())
        ;; case 3: when length don't match, don't join
        ((not (= (length x) (length y))) `(if % ,x ,y))
        ;; case 4: when double quotes, don't join
        ((or (equal? (car x) 'quote) (equal? (car y) 'quote))
         `(if % ,x ,y))
        ;; case 5: we have two if clauses
        ((and (equal? (car x) 'if) (equal? (car y) 'if))
         (cons 'if (check-list (cdr x) (cdr y))))
        ;; case 6: we have two lambda clauses
        ((and (lambda? (car x)) (lambda? (car y)))
         (lambda-handler x y))

        ;; case 7: nested list
        ((and (list? (car x)) (list? (car y)))
         (check-list x y))
        ;; case 7: when two heads do not agree, and one of then special clause
        ((and (not (equal? (car x) (car y))) (or (or (lambda? (car x)) (lambda? (car y)))
                                            (or (equal? (car x) 'if) (equal? (car y) 'if))
                                            (or (list? (car x)) (list? (car y)))))
         `(if % ,x ,y))
        ;; case 8: we have two plain lists that may or may not contain
        ;;        special clauses later on
         (else (check-list x y))   

            
         ))

(define (expr-compare x y)
  (cond  ;; case 1: both boolean
         ((and (boolean? x) (boolean? y)) (two-boolean x y))
         ;; case 2: both number
         ((and (number? x) (number? y)) (two-num x y))
         ;; when both are list
         ((and (list? x) (list? y))
          (check-clause x y))
         ((or (not (list? x)) (not (list? y))) (list 'if '% x y))

         ))

;;;;;;;;;;;;;;;;;;;;;;;;; part 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-expr-compare x y) 
  (and (equal? (eval x)
               (eval `(let ((% #t)) ,(expr-compare x y))))
       (equal? (eval y)
               (eval `(let ((% #f)) ,(expr-compare x y))))))

(define test-expr-x
  `(cons (quote x)'(cons #t (cons 12 ((lambda (a) (+ a 1)) (if 's 2 3))))))

(define test-expr-y
  `(cons (quote (y x)) (cons #f (cons 11 ((lambda (a) (+ a 2)) (if 'l 3 2))))))
