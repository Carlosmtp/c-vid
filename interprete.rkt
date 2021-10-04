#lang eopl
(provide (all-defined-out))
(require "./gramatica.rkt")
;Proyecto Final: Fundamentos de Lenguajes de Programación
;
;Desarrolladores:
;;Diana Katherine Toro Ortiz - 2110046
;;Carlos Mauricio Tovar Parra - 1741699
;;Nicolás Jaramillo Mayor - 1840558

;------------------------------------------INTERPRETE---------------------------------------

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (unparse-programa pgm))
                        (sllgen:make-stream-parser 
                         lexica
                         gramatica)))


;------------------------------------------REFERENCIAS---------------------------------------
;definición del tipo de una referencia
(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;dere!: ref -> procedimiento
;purpose: funcion que llama a primitive-deref con el argumento
(define deref
  (lambda (ref)
    (primitive-deref ref)))

;primitive-deref: ref -> el valor de una referencia
;purpose: funcion que recibe una referencia y retorna un valor
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;setref!: ref * val * env -> procedimiento
;purpose: funcion que recibe una referencia, un valor y el ambiente y se los pasa como argumento el primitive-setref!
(define setref!
  (lambda (ref val env)
    (primitive-setref! ref (unparse-expresion val env))))

;primitive-setref!: ref * val -> procedimiento
;purpose: funcion que cambia el valor al que apunta una referencia en el ambiente
(define primitive-setref!
     (lambda (ref val)
       (cases reference ref
         (a-ref (pos vec)
                (vector-set! vec pos val)))))


;------------------------------------------AMBIENTES---------------------------------------

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env)  ;función que crea un ambiente vacío
  (extended-env-record (syms (list-of symbol?))
                       (vec vector?)
                       (env environment?))
   (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expresion?))
                                   (env environment?)))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define init-env
  (lambda ()
    (empty-env)))

;crea un vector que contiene el ambiente del programa
(define env-programa
 (vector (init-env) '()))

;set-env: v -> ambiente actualizado
;purpose: actualiza el ambiente del programa contenido en el vector env-programa
(define set-env
  (lambda (v)
    (begin
    (vector-set! env-programa 0 v)
    (vector-ref env-programa 0))))

;set-env-sta-aux: ids * v -> ambiente
;purpose: funcion que cambia el ambiente agregando el static siempre y cuando no exista 
(define set-env-sta
  (lambda (ids v)
    (set-env-sta-aux ids)
    (vector-set! env-programa 0 v)
    (vector-ref env-programa 0)))

;set-env-sta-aux: ids -> 0 o error
;purpose: funcion que busca en la lista de estatica la variable estatica, si existe genera error, si no, se agrega 
(define set-env-sta-aux
  (lambda (ids)
    (cond
      [(null? ids) 0]
      [(list-find-position (car ids) (vector-ref env-programa 1))
        (eopl:error 'static "~s is static and cannot be changed." (car ids))]
      [else
        (begin
          (vector-set! env-programa 1 (cons (car ids) (vector-ref env-programa 1)))
          (set-env-sta-aux (cdr ids))
          )]
      )))

;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;purpose: función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env))) 

;apply-env: env * sym -> valor
;purpose: funcion que recibe un simbolo que busca en el ambiente y retorna una valor
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;apply-env-ref: env * sym -> referencia
;purpose: funcion que recibe un simbolo que busca en el ambiente y retorna una referencia
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vec env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vec)
                                 (apply-env-ref env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env-ref old-env sym)))))))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;purpose: función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))


;Funciones auxiliares ambientes

;list-find-position: sym * los -> procedimientos
;purpose: funcion que le pasa los argumentos a list-index
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;list-index: pred * ls -> lista
;purpose: funcion que encuentra la posicion de un simbolo en una lista  
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))



;------------------------------------------PROCEDIMIENTOS---------------------------------------

;usage: se construye un procval (un procedimiento) con datatypes
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

(define-datatype function function?
  (fun
   (id symbol?)
   (args (list-of expresion?))
   (exp expresion?)))

;apply-procedure: proc * args -> closure
;purpose: funcion definir un procedimiento  
;usage: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (unparse-expresion body (extend-env ids args env))))))


;------------------------------------UNPARSE PARA INTERPRETE-----------------------------------
;unparse-programa: pgm -> programa
;purpose: funcion unparse para programa  
;usage: interpreta y ejecuta un programa del lenguaje haciendo unparse a sus elementos
(define unparse-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (exp)
                    (unparse-expresion exp (vector-ref env-programa 0))))))

;unparse-expresion: exp * env -> expresion
;purpose: funcion unparse para expresion  
;usage: interpreta y ejecuta una expresion del lenguaje
(define unparse-expresion
  (lambda (exp env)
    (cond
      [(number? exp) exp]
      [(procval? exp) exp]
      [(function? exp)
       (cases function exp
         (fun (id args e) )
      )]
      [(vector? exp) exp]
      [(list? exp) exp]
      [else
    (cases expresion exp
      (glob-list-exp (ids exps body)
          (unparse-expresion body
             (set-env
              (extend-env
                                  ids
                                  (unparse-rands exps env)
                                  env))
                             ))
      (id-exp (id)
              (unparse-expresion
               (let ([aux (apply-env-ref env id)])
                 (if (reference? aux) (deref aux) aux))
               env))
      (ref-id-exp (id) (unparse-ref(apply-env-ref env id)))
      (var-exp (ids exps body)
               (unparse-expresion body  (set-env(extend-env ids (unparse-rands exps env) env))))
      (sta-exp (ids vals body)
               (unparse-expresion body  (set-env-sta ids (extend-env ids (unparse-rands vals env) env))))
      (rec-exp (proc-names idss bodies rec-body)
               (unparse-expresion rec-body
                                  (extend-env-recursively proc-names idss bodies env)))
      (c-vid-val-exp () "@value")
      (oct-exp (octal)  (cons 'x8 octal))
      (num-exp (num) num)
      (cara-exp (caracter) caracter)
      (cad-exp (cadena) cadena)
      (oper-exp (exp prim) ((unparse-arit-prim prim env) (unparse-expresion exp env)))
      (oper-exp-oct (exp prim)
           (unparse-expresion (oct-exp ((unparse-arit-prim-octal prim env) (cdr(unparse-expresion  exp env)))) env))
      (true-exp () #t)
      (false-exp () #f)
      (bool-comp-exp (num1 pred num2)
                     ((unparse-pred-prim pred env)
                      (unparse-expresion num1 env)
                      (unparse-expresion num2 env)))
      (bool-oper-exp (oper exp1 exp2)
                     ((unparse-oper-bin-bool oper)
                      (unparse-expresion exp1 env)
                      (unparse-expresion exp2 env)))
      (not-bool-exp (bool-value) (not (unparse-expresion bool-value env)))
      (pred-bool-exp (expr-bool) (unparse-expr-bool expr-bool env))
      (list-exp (lista) (unparse-list lista env))
      (pred-list (lprim e) (unparse-list-prim lprim (unparse-expresion e env)))
      (vec-exp (vector) (unparse-vec vector env))
      (pred-vect (vect-prim) (unparse-vect-prim vect-prim env))
      (reg-exp (registro) (unparse-reg registro env))
      (pred-registro (reg-prim) (unparse-reg-prim reg-prim env))
      (seq-exp (exp1 exps)
              (let loop ((acc (unparse-expresion exp1 env))
                   (exps exps))
                (if (null? exps) acc
                    (loop (unparse-expresion (car exps) env) (cdr exps)))) )
      (if-exp (test-exp true-exp false-exp)
              (if (unparse-expresion test-exp env)
                  (unparse-expresion true-exp env)
                  (unparse-expresion false-exp env)))
      (cond-exp (conds exps els) (cond-aux conds exps els env))
      (while-exp (condition body)
                 (while-aux condition body env))
      (for-exp (id exp1 prim exp2 body)
               (for-aux id exp1 prim exp2 body (extend-env (list id) (list (unparse-expresion exp1 env)) env)))
      (app-exp (rator rands)
               (let ((proc (unparse-expresion rator env))
                     (args (unparse-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expresion
                                 "Attempt to apply non-procedure ~s" proc))))
      (funcion (id args exp)
            (set-env
              (extend-env
                                  (list id)
                                  (list (fun id args exp))
                                  env))
               )
      (set-exp (id exp)
               (setref! (unparse-ref(apply-env-ref env id)) exp env))
      (else -1))])))

(define cond-aux
  (lambda (conds exps els env)
    (cond
      [(null? conds) (unparse-expresion els env)]
      [(unparse-expresion (car conds) env)
       (unparse-expresion (car exps) env)]
      [else (cond-aux (cdr conds) (cdr exps) els env)])
    ))

;unparse-for-prim: prim -> string
;purpose: funcion unparse para el indicador del for para aumentar o disminuir 
;usage: convierte un indicador para el for del lenguaje de sintaxis abstracta a sintaxis concreta
(define unparse-for-prim
  (lambda (prim)
    (cases for-prim prim
      (to-exp () "to")
      (downto-exp () "downto"))))

;unparse-ref: ref -> referencia 
;purpose: funcion unparse para una referencia 
;usage: convierte una referencia de sintaxis abstracta a sintaxis concreta
(define unparse-ref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec) (a-ref pos vec)))))

;unparse-pred-prim: boolprim * env -> procedimiento de racket 
;purpose: funcion unparse para operadores de comparacion 
;usage: interpreta un predicado de comparacion del lenguaje y retorna el procedimiento respectivo de racket
(define unparse-pred-prim
  (lambda (boolprim env)
    (cases pred-prim boolprim
    (menor () <) 
    (mayor () >)
    (menor-igual () <=)
    (mayor-igual () >=)
    (igual () equal?)
    (entre-cerrado (n)(lambda (i f)
                (and (>=
                      (unparse-expresion n env) i)
                     (<= (unparse-expresion n env) f))))
    (entre-abierto (n) (lambda (i f)
                (and (>
                      (unparse-expresion n env) i)
                     (< (unparse-expresion n env) f)))))))

;unparse-list: lst * env -> lista 
;purpose: funcion unparse para crear las listas
;usage: interpreta una lista del lenguaje y retorna una lista de racket
(define unparse-list
  (lambda (lst env)
    (cases lista lst
      (list-e (elems) (map (lambda (i) (unparse-expresion i env)) elems))
      (empt-list () '())
      (cons-list (e l)
                 (cons (unparse-expresion e env)
                       (unparse-expresion l env)))
      (append-list (l1 l2)
                   (append (unparse-expresion l1 env)
                           (unparse-expresion l2 env)))
      )
    ))

;unparse-list-prim: lprim * exp -> lista o un elemento de una lista
;purpose: funcion unparse para primitivas de listas
;usage: interpreta y ejecuta una list-prim que son las operaciones con las listas del lenguaje
(define unparse-list-prim
  (lambda (lprim exp)
    (if (null? exp) '()
    (cases list-prim lprim
      (lista-cabeza () (car exp))
      (lista-cola () (if (null? (cdr exp))
                         (car exp)
                         (unparse-list-prim lprim (cdr exp))))
      ))))

;unparse-expr-bool: exp * env -> bool
;purpose: funcion unparse para expresiones booleanas
;usage: interpreta y ejecuta una expr-bool que son los predicados del lenguaje
(define unparse-expr-bool
  (lambda (exp env)
    (cases expr-bool exp
      (lista-pred (lst) (list? (unparse-expresion lst env)))
      (lista-vacia-pred (lst) (null? (unparse-expresion lst env)))
      (vect-pred (vec)(vector? (unparse-expresion vec env)))
      (registro-pred (reg)
        (let ([r (unparse-expresion reg env)] )
         (cond
          [(null? r) #f]
          [(and
            (list? r)
            (list? (car r))
            (not (null? (cdr r)))
            (vector? (cadr r))) #t]
          [else #f])
        ))
      )))

;unparse-vect: v * env -> vector
;purpose: funcion unparse para los vectores
;usage: convierte una lista en vector 
(define unparse-vec
  (lambda (v env)
    (cases vect v
      (vec (elems) (list->vector (map (lambda (i) (unparse-expresion i env)) elems))))))

;unparse-vect-prim: v * env -> vector
;purpose: funcion unparse para las primitivas de los vectores
;usage: opera las expresiones de los vectores
(define unparse-vect-prim
  (lambda (v env)
    (cases vect-prim v
      (vect-ref (n vec)
                (vector-ref (unparse-expresion vec env) n))
      (vect-set (val n vec)
                  (vector-set!
                    (unparse-expresion vec env)
                    n
                    (unparse-expresion val env))
      ))))

;unparse-reg-prim: reg * env -> registro
;purpose: funcion unparse para las primitivas de los registros
;usage: opera las expresiones de los registros
(define unparse-reg
  (lambda (reg env)
    (cases registro reg
      (regist (ids vals)
              (list
               ids
               (list->vector
                (map (lambda (i) (unparse-expresion i env)) vals)))
      ))))

;unparse-reg-prim: reg * env -> registro
;purpose: funcion unparse para las primitivas de los registros
;usage: opera las expresiones de los registros
(define unparse-reg-prim
  (lambda (reg env)
    (cases reg-prim reg
      (registro-ref (id re)
              (let ([r (unparse-expresion re env)] )
                  (vector-ref (cadr r) (list-find-position id (car r)))
      ))
      (registro-set (val id re)
                    (let ([r (unparse-expresion re env)] )
                  (vector-set! (cadr r) (list-find-position id (car r)) (unparse-expresion val env))
      )))))

;unparse-arit-prim: prim * env -> numero
;purpose: funcion unparse para las primitivas aritmeticas de los numeros
;usage: opera las expresiones de los numeros
(define unparse-arit-prim
  (lambda (prim env)
    (cases arit-prim prim
      (suma (expresion)  (lambda(n) (+ n (unparse-expresion expresion env))))
      (resta (expresion) (lambda(n) (- n (unparse-expresion expresion env))))
      (multiplicacion (expresion) (lambda(n) (* n (unparse-expresion expresion env))))
      (division       (expresion) (lambda(n) (/ n (unparse-expresion expresion env))))
      (aumentar  () (lambda(n) (+ n 1)))
      (disminuir () (lambda(n) (- n 1))))))

;unparse-arit-prim-octal: prim * env -> octal
;purpose: funcion unparse para las primitivas de los octales
;usage: opera las expresiones de los octales 
(define unparse-arit-prim-octal
  (lambda (prim env)
    (cases arit-prim-octal prim
      (suma-octal  (expresion) (lambda(n) (sumaOctal n (cdr(unparse-expresion expresion env)))))
      (resta-octal (expresion) (lambda(n) (restaOctal n (cdr(unparse-expresion expresion env)))))
      (multiplicacion-octal (expresion) (lambda(n) (multiplicacionOctal n (cdr(unparse-expresion expresion env)))))
      (aumentar-octal  () (lambda(n) (successor n )))
      (disminuir-octal () (lambda(n) (predecessor  n))))))

;unparse-oper-bin-bool: oper -> bool
;purpose: funcion unparse para los operadores binarios booleanos
;usage: evalua dos elementos con los operadores booleanos 
(define unparse-oper-bin-bool
  (lambda (oper)
    (cases oper-bin-bool oper
      (and-oper () (lambda (p q) (and p q)))
      (or-oper  () (lambda (p q) (or p q)))
      (xor-oper () (lambda (p q) (not (equal? p q)))))))

;------------------------------FUNCIONES AUXILIARES PARA UNPARSE-EXPRESION----------------------------------------

;unparse-rands: rands * env -> <void>
;purpose: funcion auxiliar para recorrer los operandos
;usage: funcion auxiliar para aplicar unparse-expresion a cada elemento de una separated-list
(define unparse-rands
  (lambda (rands env)
    (map (lambda (i) (unparse-expresion i env)) rands)))

;while-aux: con * body * env -> <void>
;purpose: funcion auxiliar para hacer un bucle para el whilee 
(define while-aux
  (lambda (con body env)
    (if (unparse-expresion con env)
        (begin
          (unparse-expresion body env)
          (while-aux con body env))
        '())))

;for-aux: id * exp1 * prim * exp2 * body * env -> <void>
;purpose: funcion auxiliar para hacer un bucle para el for
;usage: for-aux retorna de acuerdo a la entrada de la primitiva, si es to aumenta exp1 hasta que sea igual a exp2
; si es downto disminuye exp1 hasta que sea igual a exp2
(define for-aux
  (lambda (id exp1 prim exp2 body env)    
    (cond
      [(equal? (unparse-for-prim prim) "to")
         (if (< (apply-env env id)(unparse-expresion exp2 env))
             (begin
               (unparse-expresion body env)
               (setref! (unparse-ref(apply-env-ref env id)) (+ (unparse-expresion exp1 env) 1) env)
               (for-aux id (apply-env env id) prim exp2 body env))
             '())]
      [(equal? (unparse-for-prim prim) "downto")
         (if (> (apply-env env id)(unparse-expresion exp2 env))
             (begin
               (unparse-expresion body env)
               (setref! (unparse-ref(apply-env-ref env id)) (- (unparse-expresion exp1 env) 1) env)
               (for-aux id (apply-env env id) prim exp2 body env))
             '())])))

;numlist->string: lista de numeros -> string
;purpose: convertir una lista de numeros a string
;usage: numlist->string retorna un string
(define numlist->string
  (lambda (l)
    (cond
      [(null? l) ""]
      [(null? (cdr l)) (number->string(car l))]
      [else (string-append
             (number->string(car l))
             " "
             (numlist->string (cdr l)))]
      )))

;----------------------------------------------------------------------------------------------------

;Base
(define N 8)

;----------------------------------------------------------------------------------------------------

;zero: void -> null list
;purpose: definición de n=0
;usage: zero() retorna una lista vacía
(define zero (lambda () '()))

;----------------------------------------------------------------------------------------------------

;is-zero?: list -> boolean
;purpose: verificar si la lista ingresada es zero
;usage: (is-zero? n) retorna #t si n es zero, de lo contrario retorna #f
(define is-zero? (lambda (n) (or (null? n) (equal? n '(0)))))

;----------------------------------------------------------------------------------------------------

;valid-in-N-base?: list -> boolean
;purpose: verificar si la lista ingresada es un n válido en la base N
;usage: (valid-in-N-base n) retorna #t si n es válido en la base N, de lo contrario retorna #f
(define valid-in-N-base?
  (lambda (n)
    (cond
      [(is-zero? n) #t]
      [(>= (car n) N) #f]
      [(<(car n) N) (valid-in-N-base? (cdr n))])))

;----------------------------------------------------------------------------------------------------

;sucessor: list -> list
;purpose: retornar el sucesor de n en base N
;usage (successor n) retrona una lista con el sucesor de n si es una
;representación válida en N, de lo contrario retorna un error
(define successor
  (lambda (n)
    (cond
      [(is-zero? n) (list 1)]
      [(valid-in-N-base? n)
       (cond
         [(= (car n) (- N 1))
          (cons 0 (successor (cdr n)))]
         [(< (car n) (- N 1)) (cons (+ (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))

;----------------------------------------------------------------------------------------------------

;predecessor: list -> list
;purpose: retornar el predecesor de n en base N
;usage (successor n) retrona una lista con el predecesor de n si es una
;representación válida en N, de lo contrario retorna un error
(define predecessor
    (lambda (n)
    (cond
      [(is-zero? n) (eopl:error 'Bignum "No se pueden representar números menores que 0")]
      [(valid-in-N-base? n)
       (cond
         [(equal? n (successor '())) '()]
         [(= (car n) 0) (cons (- N 1) (predecessor (cdr n)))]
         [else (cons (- (car n) 1 ) (cdr n))])]
      [else (eopl:error 'Bignum "No se pueden representar números con dígitos mayores o iguales a N")])))


;------------------------------------------------------------------------------------------

;sumaOctal: list -> list
;purpose: sumar dos n en base N
;usage (suma x y) retorna la suma en base N de x e y
(define sumaOctal
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (sumaOctal (predecessor x) y)))))

;----------------------------------------------------------------------------------------------------

;restaOctal: list -> list
;purpose: restar dos n en base N
;usage (resta x y) retorna la resta en base N de x e y
(define restaOctal
  (lambda (x y)
    (if (is-zero? y)
        x
        (predecessor (restaOctal  x (predecessor y))))))

;----------------------------------------------------------------------------------------------------

;multiplicacion: list -> list
;purpose: multiplicar dos n en base N
;usage (multiplicacion x y) retorna la multiplicación en base N de x e y
(define multiplicacionOctal
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (sumaOctal (multiplicacionOctal (predecessor x) y) y))
    ))

(interpreter)