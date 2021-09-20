#lang eopl
(provide (all-defined-out))
(require "./gramatica.rkt")

;El intérprete

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (unparse-programa pgm))
                        (sllgen:make-stream-parser 
                         lexica
                         gramatica)))

; Referencias
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
     (lambda (ref val)
       (cases reference ref
         (a-ref (pos vec)
                (vector-set! vec pos val)))))


;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env)  ;función que crea un ambiente vacío
  (extended-env-record (syms (list-of symbol?))
                       (vec vector?)
                       (env environment?)))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define init-env
  (lambda ()
    (empty-env)))


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vec env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vec)
                                 (apply-env-ref env sym)))))))


;Funciones auxiliares

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))



;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (unparse-expresion body (extend-env ids args env))))))



;****************************************************************

(define unparse-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp)
                    (unparse-expresion exp (init-env))))))




(define unparse-expresion
  (lambda (exp env)
    (if (number? exp) exp
    (cases expresion exp
      (glob-list-exp (ids exps exp)
          (unparse-expresion exp (extended-env-record
                                  ids
                                  (list->vector (map (lambda (i) (unparse-expresion i env)) exps))
                                  env)))
      (id-exp (id) (unparse-expresion(apply-env env id) env))
      (ref-id-exp (id) (unparse-ref(apply-env-ref env id))) ;(string-append "&" (symbol->string id)))
      (var-exp (ids exps cuerpo)
               (string-append
                "var("
                (symbol->string (car ids))
                "="
                (let ((args (unparse-rands ids env)))
                 (unparse-expresion cuerpo (extend-env ids args env)))
               ))
      (c-vid-val-exp () "@value")
      (oct-exp (octal) (string-append "x8(" (number->string(car octal))")"))
      (num-exp (num) num)
      (cara-exp (caracter) caracter)
      (cad-exp (cadena) cadena)
      (true-exp () #t)
      (false-exp () #f)
      (bool-comp-exp (num1 pred num2)
                     ((unparse-pred-prim pred)
                      (unparse-expresion num1 env)
                      (unparse-expresion num2 env)))
      (bool-oper-exp (oper exp1 exp2)
                     ((unparse-oper-bin-bool oper)
                      (unparse-expresion exp1 env)
                      (unparse-expresion exp2 env)))
      (not-bool-exp (bool-value) (not (unparse-expresion bool-value env)))
      (list-exp (lista) lista)
      (vec-exp (vector) vector)
      (reg-exp (registro) registro)
      ;(expr-bool-exp (expr-bool) expr-bool)
      (if-exp (test-exp true-exp false-exp)
              (if (unparse-expresion test-exp env)
                  (unparse-expresion true-exp env)
                  (unparse-expresion false-exp env)))
      (app-exp (rator rands)
               (let ((proc (unparse-expresion rator env))
                     (args (unparse-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      (else 1)))));continuar!!!!

(define unparse-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (list pos vec)))))

(define unparse-pred-prim
  (lambda (boolprim)
    (cases pred-prim boolprim
    (menor () <) 
    (mayor () >)
    (menor-igual () <=)
    (mayor-igual () >=)
    (igual () equal?)
    (entre () (lambda (n i f)
                (and (>= n i) (<= n f)))))))

(define unparse-oper-bin-bool
  (lambda (oper)
    (cases oper-bin-bool oper
      (and-oper () (lambda (p q) (and p q)))
      (or-oper () (lambda (p q) (or p q)))
      (xor-oper () (lambda (p q) (not (equal? p q)))))))

; funciones auxiliares para aplicar unparse-expresion a cada elemento de una 
;separated-list
(define unparse-rands
  (lambda (rands env)
    (car (map (lambda (i) (unparse-expresion i env)) rands))))