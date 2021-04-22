#lang eopl
;Proyecto Final: Fundamentos de Lenguajes de Programación
;
;Desarrolladores:
;;Diana Katherine Toro Ortiz - 2110046
;;Carlos Mauricio Tovar Parra - 1741699
;;Nicolás Jaramillo Mayor - 1840558

;******************************************************************************************
;Especificación Léxica

(define lexica
'(
    (white-sp (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit))) symbol)
    (letra (letter) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)  "." digit (arbno digit)) number)
    (octal ("x8" "("  (arbno digit) ")") number)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (arbno letter) "\"") string)
    (bool ((or "true" "false")) string)
    (for ((or "to" "downto")) string)
   )
)

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    (programa (globales expresion) un-programa)    
    (globales ("(" (separated-list identificador "=" expresion ",") ")" ) glob-exp)
    (expresion (identificador) id-exp)
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion) var-exp)
    (expresion ("cons"(separated-list identificador "=" expresion ",") "in" expresion) cons-exp)
    (expresion ("rec" (arbno identificador (separated-list identificador ",") "=" expresion) "in" expresion) rec-exp)
    (expresion ("unic" (separated-list identificador "=" expresion ",") "in" expresion) unic-exp)
    (expresion (octal) oct-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (lista) list-exp)
    (expresion (vector) vec-exp)
    (expresion (registro) reg-exp)
    (expresion (expr-bool) boolean-exp)
    (expresion ("sequence" expresion ";" (arbno expresion ";") "end") seq-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("cond" (arbno "["expresion expresion"]") "else" expresion "end") cond-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    (expresion ("for" identificador "=" expresion for expresion "do" expresion "done") for-to-exp)
    (lista ("["(arbno expresion ";")"]") list)
    (vector ("vector" "["(arbno expresion ";")"]") vec)
    (registro ( "(" identificador "=" expresion ";" (arbno identificador "=" expresion ";") ")" ) regist)
    (expr-bool ("compare" "(" expresion pred-prim expresion ")" ) exprBool); cambiar nombre
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") exprBool1); cambiar nombre
    (expr-bool (bool) exprBool2)
    (expr-bool (oper-un-bool "(" expr-bool ")") exprBool3); cambiar nombre
    (pred-prim ("<") menor)
    (pred-prim (">") mayor)
    (pred-prim ("<=") menor-igual)
    (pred-prim (">=") mayor-igual)
    (oper-bin-bool ("and") and)
    (oper-bin-bool ("or") or)
    (oper-bin-bool ("xor") xor)
    (oper-un-bool ("not") not)
    (arit-prim-10 ("+") suma)
    (arit-prim-10 ("-") resta)
    (arit-prim-10 ("*") multiplicacion)
    (arit-prim-10 ("/") division)
    (arit-prim-10 ("++") aumentar)
    (arit-prim-10 ("--") disminuir)
    (arit-prim-8 ("+") octal-suma)
    (arit-prim-8 ("-") octal-resta)
    (arit-prim-8 ("*") octal-multiplicacion)
    (arit-prim-8 ("++") octal-aumentar)
    (arit-prim-8 ("--") octal-disminuir)
    (cad-prim ("longitud") cadena-long)
    (cad-prim ("concatenar") cadena-con)
    (list-prim ("vacio") lista-vacia)
    (list-prim ("vacio?") lista-vacia-pred)
    (list-prim ("crear-lista") lista-crear)
    (list-prim ("lista?") lista-pred)
    (list-prim ("cabeza") lista-cabeza)
    (list-prim ("cola") lista-cola)
    (list-prim ("append") lista-append)
    (vect-prim ("vector?") vector-pred)
    (vect-prim ("crear-vector") vector-crear)
    (vect-prim ("ref-vector") vector-ref)
    (vect-prim ("set-vector") vector-set)
    (reg-prim ("registros?") registro-pred)
    (reg-prim ("crear-registro") registro-crear)
    (reg-prim ("ref-registro") registro-ref)
    (reg-prim ("set-registro") registro-set)
    ))

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

(define unparse-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (glob exp)
                   (unparse-globales glob)
                   (unparse-expresion exp)))))
;(unparse-programa (scan&parse "(ix = 2,) 3"))

(define unparse-globales
  (lambda (globals)
    (cases globales globals
      (glob-exp (ids exp)
                (symbol->string (car ids))
                (unparse-expresion exp)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (id-exp (id) (symbol->string (car id)))
      ;() ;falta!!!!
      (oct-exp (octal) octal)
      (num-exp (num) (number->string num))
      (cara-exp (caracter) caracter)
      (cad-exp (cadena) cadena)
      (boolean-exp (bool) bool)
      (list-exp (lista) lista)
      (vec-exp (vector) vector)
      (reg-exp (registro) registro)
      (else 1))));continuar!!!!
      


(define unparse-pred-prim
  (lambda (cadena)
    (cases pred-prim cadena
      (menor () "<")
      (mayor () ">")
      (menor-igual () "<=")
      (mayor-igual () ">="))))


(define unparse-oper-bin-bool
  (lambda (cadena)
    (cases oper-bin-bool cadena
    (and () "and")
    (or () "or")
    (xor () "xor")
    )))

(define unparse-oper-un-bool
  (lambda (cadena)
    (cases oper-un-bool cadena
      (not () "not"))))


(define unparse-prim-10
  (lambda (cadena)
    (cases arit-prim-10 cadena
      (suma () "+")
      (resta () "-")
      (multiplicacion () "*")
      (division () "/")
      (aumentar () "++")
      (disminuir () "--"))))


(define unparse-arit-prim-8
  (lambda (cadena)
    (cases arit-prim-8 cadena
      (octal-suma () "+")
      (octal-resta () "-")
      (octal-multiplicacion () "*")
      (octal-aumentar () "++")
      (octal-disminuir () "--"))))

(define unparse-cad-prim
  (lambda (cadena)
    (cases cad-prim cadena
      (cadena-long () "longitud")
      (cadena-con () "concatenar"))))

(define unparse-list-prim
  (lambda (cadena)
    (cases list-prim cadena
      (lista-vacia () "vacio")
      (lista-vacia-pred () "vacio?")
      (lista-crear () "crear-lista")
      (lista-pred () "lista?")
      (lista-cabeza () "cabeza")
      (lista-cola () "cola")
      (lista-append () "append"))))


(define unparse-vect-prim
  (lambda (cadena)
    (cases vect-prim cadena
      (vector-pred () "vector?")
      (vector-crear () "crear-vector")
      (vector-ref () "ref-vector")
      (vector-set () "set-vector"))))

(define unparse-reg-prim
  (lambda (cadena)
    (cases reg-prim cadena
      (registro-pred () "registro?")
      (registro-crear () "crear-registro")
      (registro-ref () "ref-registro")
      (registro-set () "set-registro"))))


