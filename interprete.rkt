#lang eopl
(provide (all-defined-out))
(require "./gramatica.rkt")


;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío

(define init-env
  (lambda ()(extend-env '() '()(empty-env))))


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env))) 

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym)))))))

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

; funciones auxiliares para aplicar unparse-expresion a cada elemento de una 
;separated-list
(define unparse-rands
  (lambda (rands env)
    (car (map (lambda (x) (unparse-rand x env)) rands))))

(define unparse-rand
  (lambda (rand env)
    (unparse-expresion rand env)))

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

;El intérprete

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (unparse-programa pgm))
                        (sllgen:make-stream-parser 
                         lexica
                         gramatica)))


(define unparse-expresion
  (lambda (exp env)
    (cases expresion exp
      (glob-list-exp (ids exps body exp) (unparse-expresion exp env))
      (id-exp (id) (apply-env env id))
      (ref-id-exp (id)(string-append "&" (symbol->string id)))
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
      (else 1))));continuar!!!!

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
