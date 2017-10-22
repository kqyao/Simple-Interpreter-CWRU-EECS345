(load "simpleParser.scm")

; (const? n) return true if n is a number or #t, #f. 
(define const?
  (lambda (n)
    (cond
      ((number? n) #t)
      ((eq? n #t) #t)
      ((eq? n #f) #t)
      (else #f))))

; (M_var_value x s): x is a variable. s is a state. return the value of the variable in state s. error if the variable is not declared
(define M_var_value
  (lambda (x s)
    (cond
      ((null? (car s)) (error "variable not declared:" x))
      ((eq? x (car (car s))) (if (eq? (car (cadr s)) '"")
                                 (error "using before assigning of variable " x)
                                 (car (cadr s))))
      (else (M_var_value x (cons (cdr (car s)) (cons (cdr (cadr s)) '())))))))

; (M_expr_value e s): where e is an expression. s is a state. return the (value of the expression, state).
(define M_expr_value
  (lambda (e s)
    (cond
      ((list? e) (cond
                   ((eq? (car e) '+) (cons (+ (car (M_expr_value (cadr e) s)) (car (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))) (cdr (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))))
                   ((eq? (car e) '-) (if (null? (cdr (cdr e)))
                                         (cons (- 0 (car (M_expr_value (cadr e) s))) (cdr (M_expr_value (cadr e) s)))
                                         (cons (- (car (M_expr_value (cadr e) s)) (car (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))) (cdr (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s)))))))
                   ((eq? (car e) '*) (cons (* (car (M_expr_value (cadr e) s)) (car (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))) (cdr (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))))
                   ((eq? (car e) '/) (cons (quotient (car (M_expr_value (cadr e) s)) (car (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))) (cdr (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))))
                   ((eq? (car e) '%) (cons (% (car (M_expr_value (cadr e) s)) (car (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))) (cdr (M_expr_value (caddr e) (cdr (M_expr_value (cadr e) s))))))
                   ((eq? (car e) '=) (cons (car (M_expr_value (caddr e) s)) (assign_value (cadr e) (car (M_expr_value (caddr e) s)) (cdr (M_expr_value (caddr e) s)))))
                   ((or (eq? (car e) '==) (or (eq? (car e) '!=) (or (eq? (car e) '>) (or (eq? (car e) '<) (or (eq? (car e) '>=) (or (eq? (car e) '<=) (or (eq? (car e) '&&) (or (eq? (car e) '||) (eq? (car e) '!)))))))))
                    (cons (car (M_boolean e s)) (cdr (M_boolean e s))))
                   (else (error "wrong expression: " e))))
      ((number? e) (cons e s))
      (else (cons (M_var_value e s) s)))))

; (assign_value x n s) change the value of x in s to n. return the new state
(define assign_value
  (lambda (x n s)
    (cond
      ((null? (car s)) (error "variable not declared:" x))
      ((eq? x (car (car s))) (cons (car s) (cons (cons n (cdr (cadr s))) '())))
      (else (cons (car s) (cons (cons (car (car (cdr s))) (cadr (assign_value x n (cons (cdr (car s)) (cons (cdr (cadr s)) '()))))) '()))))))

; (% a b)
(define %
  (lambda (a b)
    (- a (* b (quotient a b)))))

; (M_boolean b s) return (#t|#f, s)
(define M_boolean
  (lambda (b s)
    (cond
      ((eq? b 'true) (cons #t s))
      ((eq? b 'false) (cons #f s))
      ((list? b) (cond
                   ((eq? (car b) '==) (cons (eq? (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '!=) (cons (not (eq? (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s)))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '<) (cons (< (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '>) (cons (> (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '<=) (cons (<= (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '>=) (cons (>= (car (M_expr_value (cadr b) s)) (car (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))) (cdr (M_expr_value (caddr b) (cdr (M_expr_value (cadr b) s))))))
                   ((eq? (car b) '&&) (cons (and (car (M_boolean (cadr b) s)) (car (M_boolean (caddr b) (cdr (M_boolean (cadr b) s))))) (cdr (M_boolean (caddr b) (cdr (M_boolean (cadr b) s))))))
                   ((eq? (car b) '||) (cons (or (car (M_boolean (cadr b) s)) (car (M_boolean (caddr b) (cdr (M_boolean (cadr b) s))))) (cdr (M_boolean (caddr b) (cdr (M_boolean (cadr b) s))))))
                   ((eq? (car b) '!) (cons (not (car (M_boolean (cadr b) s))) (cdr (M_boolean (cadr b) s))))
                   (else (error "wrong condition: " b))))
      (else (cons (M_var_value b s) s)))))

(define in?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq? x (car l)) #t)
      (else (in? x (cdr l))))))

; (M_declare d s) declare variable using declare statement d with state s, return the new state
(define M_declare
  (lambda (d s)
    (if (in? (cadr d) (car s))
        (error "redefine variable: " (cadr d))
        (if (null? (cdr (cdr d)))
            (cons (cons (cadr d) (car s)) (cons (cons '"" (cadr s)) '()))
            (cons (cons (cadr d) (car s)) (cons (cons (car (M_expr_value (caddr d) s)) (cadr (cdr (M_expr_value (caddr d) s)))) '()))))))

; (M_if i s r_b r_v) i is the if statement. s is the state, r_b is return boolean, r_v is return value
; return (s r_b r_v)
(define M_if
  (lambda (i s r_b r_v)
    (cond
      ((eq? r_b #t) (cons s (cons r_b (cons r_v '()))))
      ((car (M_boolean (cadr i) s)) (cons (car (M_state (caddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) (cons (cadr (M_state (caddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) (cons (caddr (M_state (caddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) '()))))
      (else (if (not (null? (cdddr i)))
                (cons (car (M_state (cadddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) (cons (cadr (M_state (cadddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) (cons (caddr (M_state (cadddr i) (cdr (M_boolean (cadr i) s)) r_b r_v)) '())))
                (cons (cdr (M_boolean (cadr i) s)) (cons r_b (cons r_v '()))))))))

; (M_while w s r_b r_v) w is the while loop statement, s is the state, r_b is return boolean, r_v is return value
; return (s r_b r_v)
(define M_while
  (lambda (w s r_b r_v)
    (cond
      ((eq? r_b #t) (cons s (cons r_b (cons r_v '()))))
      ((car (M_boolean (cadr w) s)) (cons (car (M_while w (car (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (cadr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (caddr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v))))
                                          (cons (cadr (M_while w (car (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (cadr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (caddr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v))))
                                                (cons (caddr (M_while w (car (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (cadr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)) (caddr (M_state (caddr w) (cdr (M_boolean (cadr w) s)) r_b r_v)))) '()))))
      (else (cons (cdr (M_boolean (cadr w) s)) (cons r_b (cons r_v '())))))))

; (M_state l s r_b r_v) l is the satement, s is the state, r_b is the return boolean, r_v is return value
; return (s r_b r_v)
(define M_state
  (lambda (l s r_b r_v)
    (cond
      ((eq? r_b #t) (cons s (cons r_b (cons r_v '()))))
      ((eq? (car l) 'return) (cons (cdr (M_expr_value (cadr l) s)) (cons #t (cons (car (M_expr_value (cadr l) s)) '()))))
      ((eq? (car l) 'var) (cons (M_declare l s) (cons r_b (cons r_v '()))))
      ((eq? (car l) '=) (cons (cdr (M_expr_value l s)) (cons r_b (cons r_v '()))))
      ((eq? (car l) 'if) (cons (car (M_if l s r_b r_v)) (cons (cadr (M_if l s r_b r_v)) (cons (caddr (M_if l s r_b r_v)) '()))))
      ((eq? (car l) 'while) (cons (car (M_while l s r_b r_v)) (cons (cadr (M_while l s r_b r_v)) (cons (caddr (M_while l s r_b r_v)) '()))))
      (else (error "statement not defined: " l)))))
      
; (M_program p s r_b r_v) p is the program, s is the state, r_b is the return boolean, r_v is return value
; return (s r_b r_v)
(define M_program
  (lambda (p s r_b r_v)
    (cond
      ((or (eq? r_b #t) (null? p)) (cons s (cons r_b (cons r_v '()))))
      (else (cons (car (M_program (cdr p) (car (M_state (car p) s r_b r_v)) (cadr (M_state (car p) s r_b r_v)) (caddr (M_state (car p) s r_b r_v))))
                  (cons (cadr (M_program (cdr p) (car (M_state (car p) s r_b r_v)) (cadr (M_state (car p) s r_b r_v)) (caddr (M_state (car p) s r_b r_v))))
                        (cons (caddr (M_program (cdr p) (car (M_state (car p) s r_b r_v)) (cadr (M_state (car p) s r_b r_v)) (caddr (M_state (car p) s r_b r_v)))) '())))))))

; interpret takes the filename and return the result of the program
(define interpret
  (lambda (filename)
    (if (eq? (cadr (M_program (parser filename) '(() ()) #f 0)) #t)
        (cond
          ((eq? (caddr (M_program (parser filename) '(() ()) #f 0)) #t) 'true)
          ((eq? (caddr (M_program (parser filename) '(() ()) #f 0)) #f) 'false)
          (else (caddr (M_program (parser filename) '(() ()) #f 0))))
        (error "no return"))))
