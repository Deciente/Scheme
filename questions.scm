(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadadr x) (car(cdr (car (cdr x)))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (cons-all first rests)
  (cond
    ;((null? rests) (cons first nil))
    ((null? rests) nil)
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
  )
)

(define (zip pairs)
  (cond
  ((null? pairs) ())
  (else (cons (firstzip pairs) (zip (zip_helper pairs))))
  )
  )

(define (firstzip pairs)
  (cond
    ((null? pairs) nil)
    (else (cons (car (car pairs)) (firstzip (cdr pairs))))
  )
  )

(define (zip_helper pairs)
  (cond
  ((null? pairs) nil)
  ((null? (cdr (car pairs))) nil)
  (else (cons (cdr (car pairs)) (zip_helper (cdr pairs))))
  )
  )



;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (enum_helper s 0)
  )

(define (enum_helper s num)
  (cond
  ((equal? nil s) ())
  (else (cons (cons num (cons (car s) nil)) (enum_helper (cdr s) (+ num 1))))
  )
  )
  ; END Question 18
  
;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (cond
    ((null? denoms) ())
    ;((equal? 0 total) ())
    (
    (>= total (car denoms))
      (append
        (cond
        ((null? (list-change (- total (car denoms)) denoms)) (cons (cons (car denoms) nil) nil))
        (else (cons-all (car denoms) (list-change (- total (car denoms)) denoms)))
          )
        (list-change total (cdr denoms))
      )
    )
    (else
      (list-change total (cdr denoms))
    )
  )
)
  ; END Question 19

(define (sum elemList)
  (if
    (null? elemList)
    0
    (+ (car elemList) (sum (cdr elemList)))
  )
)


;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((quoted? expr)
         ; BEGIN Question 20
         expr
         ; END Question 20
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
          (cons form (cons params (analyze body)))
           ; END Question 20
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
           ; BEGIN Question 20
           (define param (zip values))

           (define args (car param))
           (define func (car body))
           (define vals (cadr param))
           (define f (analyze func))
           (cons (lambda args f) (map analyze vals))
           ; END Question 20
           ))
        (else
         ; BEGIN Question 20
         (map analyze expr) ;;
         ; END Question 20
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21



(analyze '(lambda (x) a (let ((a x)) a)))
