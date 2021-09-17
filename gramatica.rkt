#lang eopl
(provide (all-defined-out))
;Proyecto Final: Fundamentos de Lenguajes de Programación
;
;Desarrolladores:
;;Diana Katherine Toro Ortiz - 2110046
;;Carlos Mauricio Tovar Parra - 1741699
;;Nicolás Jaramillo Mayor - 1840558


;******************************************************************************************

;; La definición BNF para las expresiones del lenguaje:

;producciones de la léxica
;<identificador> ::= <letter> | {<letter> | 0,...,9}*
;<letter>        ::= A...Z | a...z
;<numero>        ::= [-]{0,...,9}* | [-]{0,...,9}* . {0,...,9}*
;<octal>         ::= x8 "("{0,...,7}+( )")"
;<caracter>      ::= (')<letter>(')
;<cadena>        ::= ("){<caracter>}*(")
;<bool>          ::= true | false

;<programa>      ::= <globales> <expresion>
;<globales>      ::= "("{<identificador> = <expresion>}")"*(,)

;producciones de tipo expresión
;<expresion>     ::= <identificador>
;                ::= &<identificador>
;                ::= var "(" {<identificador> = <expresion>}*(,) ")" in <expresion>
;                ::= sta "(" {<identificador> = <expresion>}*(,) ")" in <expresion>
;                ::= rec {<identificador> "(" {<identificador>}*(,) ")" = <expresion>}* in <expresion>
;                ::= @value
;                ::= unic "(" {<identificador> = <expresion>}*(,) ")" in <expresion>
;                ::= <octal>
;                ::= <numero>
;                ::= <caracter>
;                ::= <cadena>
;                ::= <lista>
;                ::= <vector>
;                ::= <registro>
;                ::= <expr-bool>
;                ::= sequence "(" {<expresion>}+(;) ")" end
;                ::= if "(" <expr-bool> ")" then <expresion> "[" else <expresion> "]" end
;                ::= cond { "[" <expresion> <expresion> "]" }* else <expresion>} end
;                ::= while "(" <exp-bool> ")" do <expresion> done
;                ::= for "(" <identificador> = <expresion> ")" (to | downto) <expresion> do <expresion> done

;expresiones adicionales 
;<expresion>     ::= ":" "(" <expresion> <arit-prim> ")"
;                ::= "o:" "(" <expresion> <arit-prim-octal> ")"
;                ::= <cad-prim>
;                ::= <list-prim> "(" <expresion> ")"
;                ::= <vect-prim> 
;                ::= <reg-prim> 
;                ::= define <identificador> "lambda" "(" {<expresion>}* ")" <expresion>       
;                ::= set <identificador> "=" <expresion>
;                ::= "(" <expresion> {<expresion>}* ")"
 
;primitivas de cadenas
;<cad-prim>      ::= longitud "(" <expresion> ")"
;<cad-prim>      ::= concatenar "(" {<expresion>}*(,) ")"

;---------- LISTAS -----------
;<lista>         ::= "[" {<expresion>}*(;) "]"
;                ::= vacia
;                ::= cons "(" <expresion> <expresion> ")"
;                ::= append "(" <expresion> <expresion> ")"

;primitivas de listas
;<expr-bool>     ::= lista? "(" <expresion> ")"
;                ::= vacio? "(" <expresion> ")"
;<list-prim>     ::= cabeza | cola

;---------- VECTORES -----------
;<vector>        ::= vector "[" {<expresion>}*(;) "]"

;primitivas de vectores
;<expr-bool>     ::= vector? "(" <expresion> ")"
;<vect-prim>     ::= ref-vector "(" <numero> de <expresion> ")"
;                ::= set-vector "(" <expresion> en <numero> de <expresion> ")"

;---------- REGISTROS -----------
;<registro>      ::= "{" {<identificador> ":" <expresion>}+(,) "}"

;primitivas de registros
;<expr-bool>     ::= registro? "(" <expresion> ")"
;<vect-prim>     ::= ref-registro "(" <identificador> de <expresion> ")"
;                ::= set-registro "(" <expresion> en <identificador> de <expresion> ")"

;---------- BOOLEANOS -----------
;expresiones booleanas
;<expr-bool>     ::= <bool>
;                ::= compare "(" <expresion> <pred-prim> <expresion> ")"
;                ::= <oper-bin-bool> "(" <expr-bool> "," <expr-bool> ")"
;                ::= ¿ <expresion> <expresion>
;                ::= not "(" <expr-bool> ")"

;operadores booleanos
;<oper-bin-bool> ::= and | or | xor

;predicado de primitivas
;<pred-prim>     ::= < | > | <= | >= | == | <>


;---------- ARITMÉTICA -----------
;primitivas aritméticas para decimales
;<arit-prim>     ::= + | - | * | % | / | ++ | --

;primitivas aritméticas para octales
;<arit-prim-octal>     ::= + <expresion>
;                      ::= - <expresion>
;                      ::= * <expresion>
;                      ::= ++ <expresion>
;                      ::= -- <expresion>
 
;******************************************************************************************

;Especificación Léxica

(define lexica
'(
    (white-sp (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)  "." digit (arbno digit)) number)
    (caracter ("´"(or digit letter)) symbol)
    (cadena ("\"" (arbno (or letter digit whitespace)) "\"") string)
   )
)

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    (programa (expresion) un-programa)    
    (globales ("global") glob-exp)

    ;producciones de tipo expresión
    (expresion
     (globales "("(separated-list identificador "=" expresion ",")")") glob-list-exp)
    (expresion (identificador) id-exp)
    (expresion ("&"identificador) ref-id-exp)
    (expresion ("var" "("(separated-list identificador "=" expresion ",")")" "in" expresion) var-exp)
    (expresion ("sta""("(separated-list identificador "=" expresion ",")")" "in" expresion) sta-exp)
    (expresion ("rec" (arbno identificador "("(separated-list identificador ",")")" "=" expresion) "in" expresion) rec-exp)
    (expresion ("@value") c-vid-val-exp)
    (expresion ("unic" "("(separated-list identificador "=" expresion ",")")" "in" expresion) unic-exp)
    (expresion ("x8(" (arbno numero) ")") oct-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (lista) list-exp)
    (expresion (vector) vec-exp)
    (expresion (registro) reg-exp)
    (expresion (expr-bool) boolean-exp)
    (expresion ("sequence" "(" expresion ";" (arbno expresion ";") ")" "end") seq-exp)
    (expresion ("if" "(" expresion ")" "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("cond" (arbno "["expresion expresion"]") "else" expresion "end") cond-exp)
    (expresion ("while" "(" expr-bool ")" "do" expresion "done") while-exp)
    (expresion ("to") to-exp)
    (expresion ("downto") downto-exp)
    (expresion ("for" "(" identificador "=" expresion expresion expresion ")" "do" expresion "done") for-to-exp)

    ;expresiones adicionales 
    (expresion (":" "(" expresion arit-prim")") oper-exp)
    (expresion ("o:" "(" expresion arit-prim-octal")") oper-exp-oct)
    (expresion (cad-prim) pred-cadena)
    (expresion (list-prim "(" expresion ")") pred-list)
    (expresion (vect-prim) pred-vector)
    (expresion (reg-prim) pred-registro)
    (expresion ("define" identificador "lambda" "("(arbno expresion)")" expresion) funcion)
    (expresion ("call" identificador "("(arbno expresion)")") call-funcion)
    (expresion ("set" identificador "=" expresion) set-exp)
    (expresion ( "(" expresion (arbno expresion) ")")  app-exp)

    ;primitivas de cadenas
    (cad-prim ("longitud"  "("expresion")") cadena-long)
    (cad-prim ("concatenar" "("(separated-list expresion ",")")") cadena-con)
    
    ;---------- LISTAS -----------
    (lista ("["(separated-list expresion ";")"]") list)
    (lista ("vacia") empt-list)
    (lista ("cons" "("expresion expresion")") cons-list)
    (lista ("append" "("expresion expresion")") append-list)
    
    ;primitivas de listas
    (expr-bool ("lista?" "("expresion")" ) lista-pred)
    (expr-bool ("vacio?" "("expresion")" ) lista-vacia-pred)
    (list-prim ("cabeza") lista-cabeza)
    (list-prim ("cola") lista-cola)

    ;---------- VECTORES -----------
    (vector ("vec" "["(separated-list expresion ";")"]") vec)
    
    ;primitivas de vectores
    (expr-bool ("vector?" "("expresion")") vector-pred)
    (vect-prim ("ref-vector" "("numero "de" expresion")") vector-ref)
    (vect-prim ("set-vector" "("expresion "en" numero "de" expresion")") vector-set)

    ;---------- REGISTROS -----------
    (registro ( "{" identificador ":" expresion  (arbno "," identificador ":" expresion) "}" ) regist)

    ;primitivas de registros
    (expresion ("registro?" "("expresion")") registro-pred)
    (reg-prim ("ref-registro" "("identificador "de" expresion")") registro-ref)
    (reg-prim ("set-registro" "("expresion "en" identificador "de" expresion")") registro-set)

    ;---------- BOOLEANOS -----------
    ;expresiones booleanas
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("compare" "(" expresion pred-prim expresion ")" ) bool-comp-exp)
    (expresion (oper-bin-bool "(" expresion "," expresion ")") bool-oper-exp)
    (expresion ("¿"expresion expresion) pred-bool-exp)
    (expresion ("not" "(" expresion ")") not-bool-exp)

    ;operadores booleanos
    (oper-bin-bool ("and") and-oper)
    (oper-bin-bool ("or") or-oper)
    (oper-bin-bool ("xor") xor-oper)
    
    ;predicado de primitivas
    (pred-prim ("<") menor)
    (pred-prim (">") mayor)
    (pred-prim ("<=") menor-igual)
    (pred-prim (">=") mayor-igual)
    (pred-prim ("==") igual)
    (pred-prim ("<>") entre)

    ;---------- ARITMETICA -----------
    ;primitivas aritmeticas para decimales
    (arit-prim ("+" expresion) suma)
    (arit-prim ("-" expresion) resta)
    (arit-prim ("*" expresion) multiplicacion)
    (arit-prim ("/" expresion) division)
    (arit-prim ("++") aumentar)    
    (arit-prim ("--") disminuir)

    ;primitivas aritmeticas para octales
    (arit-prim-octal ("+" expresion) suma-octal)
    (arit-prim-octal ("-" expresion) resta-octal)
    (arit-prim-octal ("*" expresion) multiplicacion-octal)
    (arit-prim-octal ("++") aumentar-octal)    
    (arit-prim-octal ("--") disminuir-octal)
 )
)

;*******************************************************************************************
;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos automáticamente:

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes lexica gramatica)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
    (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

;El intérprete

(define interpreter
  (sllgen:make-rep-loop "--> "
                        (lambda (pgm) (unparse-programa pgm))
                        (sllgen:make-stream-parser 
                         lexica
                         gramatica)))
;******************************************************************************************

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
      (un-programa ( exp)
                    (unparse-expresion exp (init-env))))))


(define unparse-expresion
  (lambda (exp env)
    (cases expresion exp
      ;(glob-list-exp (ids exps body) ids)
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
