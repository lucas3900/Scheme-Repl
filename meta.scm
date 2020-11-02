
;; Lucas Barusek
;; meta.sch
;; 4/11/20

;; metacircular interpreter for a subset of scheme

(load-option 'format)

;; An environment is a list of one element.  That element is an association
;; list.  This allows us to add a binding to an environment.

(define (popl-error string)
  "Output a given error message and abort the program."
  ;;(error string))
   (format #t "ERROR: ~A~%" string)
   (*LOOPTOP* 'dontcare))


(define (popl-bind symbol value env)
  "Add a binding from symbol to value in the given environment.
   Shadows any previous binding of the same symbol, perhaps permanently."
  (let ((bindings (car env)))
    (set-car! env (cons (list symbol value) bindings)))
  symbol)

(define (popl-get-binding symbol env)
  "Look up the binding for a symbol in the environment.
   Used internally by env-value and popl-set! "
  (assoc symbol (car env)))

(define (env-value symbol env)
  "Return the value of a symbol bound in the given environment.  Throws an
   error if the symbol is not bound."
  (let ((pr (popl-get-binding symbol env)))
    (if pr
        (cadr pr)
        (popl-error (format #f "Symbol ~A not found" symbol)))))

(define (popl-define symbol value env)
  "Implementation of define special form.  Adds new binding to environment
   and returns the symbol that was defined."
  (popl-bind symbol value env)
  symbol)

;; popl-set!  change the value of a binding in an environment

;; pr = (x 7)
;; (set! x 9)


;; Oh, I already gave you set!  ?  Neat!  Looks like you just
;; need to add some code to popl-eval in order to hook this in.
;; --aerc 4/6/2020
(define (popl-set! symbol value env)
  "Find existing binding for symbol, and change its value to something new.
   Throws error if given symbol is not bound."
  (let* ((pr (popl-get-binding symbol env))
        (old (cadr pr)))
    (if pr
        (set-car! (cdr pr) value)
        (popl-error (format #f "No binding found for ~A" symbol))) old ))


(define *LOOPTOP* #!unspecific) ;; will be set! later to the continuation
                                ;; at the top of the popl-repl loop.


(define *TOP-ENVIRONMENT*
  (list (list))) ;; totally empty environment


;; TODO: Add to this sequence of allowed predefined functions.
(popl-define 'cons cons *TOP-ENVIRONMENT*)
(popl-define 'format format *TOP-ENVIRONMENT*)
(popl-define 'cdr cdr *TOP-ENVIRONMENT*)
(popl-define 'car car *TOP-ENVIRONMENT*)
(popl-define 'eq? eq? *TOP-ENVIRONMENT*)
(popl-define 'equal? equal? *TOP-ENVIRONMENT*)
(popl-define 'null? null? *TOP-ENVIRONMENT*)
(popl-define 'list list *TOP-ENVIRONMENT*)
(popl-define 'our-procedure? procedure? *TOP-ENVIRONMENT*)
(popl-define '+ + *TOP-ENVIRONMENT*)
(popl-define '- - *TOP-ENVIRONMENT*)
(popl-define '* * *TOP-ENVIRONMENT*)
(popl-define '/ / *TOP-ENVIRONMENT*)
(popl-define '= = *TOP-ENVIRONMENT*)
(popl-define '< < *TOP-ENVIRONMENT*)
(popl-define '<= <= *TOP-ENVIRONMENT*)
(popl-define '> > *TOP-ENVIRONMENT*)
(popl-define '>= >= *TOP-ENVIRONMENT*)


(define (popl-apply function arguments)
  "Implementation of function-calling lambdas.
   Extracts the pieces of the lambda and copies the environment,
   binds each parameter, recursively evaluates each form in the body in
   the copied environment. TODO:  needs to check to make sure number of
   arguments matches number of parameters and throw an error if not."
  (let* ((lambda-parameters (first function))
         (lambda-body (second function))
         (lambda-env (third function))
         (env (list (first lambda-env))))
    ;; N.B. previously we used map, but map doesn't guarantee evaluation
    ;; order.  (It works, but we were lucky.)  Use for-each when the
    ;; order matters and you only care about the side effects.
    (for-each (lambda (pair) (popl-bind (car pair) (cadr pair) env))
       (zip lambda-parameters arguments))
    (let ((result #!unspecific))
      (for-each (lambda (form) (set! result (popl-eval form env)))
          lambda-body)
      result)))


(define (popl-eval-let  bindings forms env)
  (let ((newenv (list (car env))))
  ;; for each pair in binding, call popl-bind to add it to this environment
    (for-each (lambda (binding)
      (popl-bind (car binding) (popl-eval (cadr binding) env) newenv)) bindings)

  ;; for each form in forms, evaluate it in a manner similar to popl-apply.
    (let ((result #!unspecific))
      (for-each (lambda (form) (set! result (popl-eval form newenv))) forms)
        result)))


(define (popl-eval-let* bindings forms env)
  (let ((env (list (car env))))
    ;; for each pair in binding, call popl-bind to add it to this environment
    (for-each (lambda (binding)
      (popl-bind (car binding) (popl-eval (cadr binding) env) env)) bindings)

    ;; for each form in forms, evaluate it in a manner similar to popl-apply.
    (let ((result #!unspecific))
      (for-each (lambda (form) (set! result (popl-eval form env)))
          forms)
      result)))


(define (popl-eval-cond expression env)
  (format #t "CONDDEBUG: Eval ~A~%" expression)
    (let ((expr (first expression)))
    (if (null? expression) #!unspecific)
    (if (eq? (car expr) 'else)
        (popl-eval (cadr expr) env)
        (if (popl-eval (car expr) env)
            (if (null? (cdr expr)) #t
              (popl-eval (cadr expr) env))
              (popl-eval-cond (cdr expression) env)))))

              (define (has-duplicates lst)
                (cond ((null? lst) #f)
                      ((member (car lst) (cdr lst)) #t)
                      (else (has-duplicates (cdr lst)))
                )
              )


(define (popl-eval expr env)
  "This is the main evaluator.  It is where you will add code to implement
   more features of our-lisp. You may also add helper functions
   in the spirit of popl-define, popl-set!, and popl-apply."
  (format #t "DEBUG: Eval ~A~%" expr)
  (cond ((or (number? expr) (boolean? expr) (null? expr) (string? expr))
         ;; numbers, booleans, null list all just evaluate to themselves
         expr)
        ((symbol? expr)
         ;; If you have a symbol to evaluate, look it up!
         (env-value expr env))
        ((pair? expr)
         ;; pair could be a lot of things.  Check each special form...
         (cond ((eq? (first expr) 'define)
                (let ((sym (second expr))
                      (val (popl-eval (third expr) env)))
                  (popl-define sym val env)))
               ((eq? (first expr) 'cond)
                 (popl-eval-cond (cdr expr) env))
               ((eq? (first expr) 'quote)
                (second expr))
               ((eq? (first expr) 'lambda)
                (if (null? (cddr expr))
                  (popl-error (format #f "Empty Body for lambda"))
                  (list (second expr) ;; formal parameters
                      (cddr expr)   ;; body
                       env)))         ;; environment
               ((eq? (first expr) 'let)  ;; (let BINDINGS FORM1 FORM2......)
                (popl-eval-let (second expr) (cddr expr) env))
               ((eq? (first expr) 'if)
                (if (popl-eval (second expr) env)
                    (popl-eval (third expr) env)
                    (popl-eval (fourth expr) env)))
               ;; (TODO:  cond, let, let*, set!, etc...)
               ((eq? (first expr) 'format)
                (cond ((equal? (length expr) 3)
                  (format (second expr) (third expr)))
                  (else (format (second expr) (third expr)
                        (popl-eval (fourth expr)  env)))))
               ((eq? (first expr) 'set!)
                  (popl-set! (second expr) (third expr) env))
               (else
                 ;; ...but if not a special form, must be a function call.
                 (if (not (popl-eval (our-procedure? (env-value (first expr) env)) env))
                     (popl-error (format #f "~A is not a function" (first expr))))
                 (let* ((items (map (lambda (form) (popl-eval form env)) expr))
                        (function (first items)))
                   ;; TODO:  perhaps some error checking here to make sure
                   ;; that the function is callable.
                    (if (procedure? function) ;; if function is built in.
                        (apply function (cdr items))
                        (if (= (length (first function))
                               (length (cdr items)))
                               (popl-apply function (cdr items))
                               (popl-error (format #f "Incorrect Number of Arguments"))))))))

        (else "I don't know")))

;; (popl-apply function (cdr items)))))))

;;(if (= (length (cadr (env-value function env)))
;;       (length (cdr items)))
;;       (popl-apply function (cdr items))
;;       (popl-error "Incorrect number of arguments")))))))"


;; 1 ]=> (load "meta.scm")


;; ;Loading "meta.scm"...
;; ;  Loading "format.com"... done
;; ;... done
;; ;Value: popl-repl


;; 1 ]=> (popl-repl)
;; H]=> (cons 2 ())
;; (2 . I don't know)  ;; heh, we need to fix this!
;; H]=> (cons 2 1)
;; (2 . 1)
;; H]=> (+ 1 (* 6 2))
;; 13
;; H]=> (exit)
;; Bye then!
;; ;Unspecified return value


;; 1 ]=>


(define (popl-repl)
  "Finally, this version of popl-repl makes the tail recursion explicit by
   using a 'let loop'"
  (call-with-current-continuation (lambda (here) (set! *LOOPTOP* here)))
  (let loop ()
    (format #t "H]=> ")
    (let ((expr (read)))
      (cond ((equal? expr '(exit)) (format #t "~A~%" "Bye then"))
            (else (format #t "~A~%" (popl-eval expr *TOP-ENVIRONMENT*))
                  (loop))))))


;; end of file
