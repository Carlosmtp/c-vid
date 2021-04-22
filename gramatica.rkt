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
    (octal ("x8" "("  digit (arbno digit) ")") number)
    (caracter ("'" letter "'") symbol)
    (cadena ("\"" (arbno letter) "\"") string)
    (bool ("\"" (or "true" "false") "\"") string)
    (for ((or "to" "downto")) string)
   )
)

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    (programa (globales expresion) un-programa)    
    (globales ("(" (arbno identificador "=" expresion ",") ")" ) glob-exp)

    (expresion (identificador) id-exp)
    (expresion ("var" (arbno identificador "=" expresion ",") "in" expresion) var-exp)
    (expresion ("cons"(arbno identificador "=" expresion ",") "in" expresion) cons-exp)
    ;rec??????????
    (expresion ("rec" (arbno identificador (arbno identificador ",") "=" expresion) "in" expresion) rec-exp)
    (expresion ("unic" (arbno identificador "=" expresion ",") "in" expresion) unic-exp)
    (expresion (octal) oct-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (bool) bool-exp)
    (expresion (lista) list-exp)
    (expresion (vector) vec-exp)
    (expresion (registro) reg-exp)
    (expresion (expr-bool) expr-bool-exp)
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
    ;(expr-bool (bool) exprBool2); cambiar nombre. creo que está entrando en conflicto con (expresion (bool) bool-exp)
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
      (un-programa (exp)))))

(define unparse-globales
  (lambda (globals)
    (cases globales globals
      (glob-exp (globals)))))

(define unparse-expresion
  (lambda (exp)
    (cases expresion exp
      (id-exp (id) id)
      ;() ;falta!!!!
      (oct-exp (octal) octal)
      (num-exp (num) num)
      (cara-exp (caracter) caracter)
      (cad-exp (cadena) cadena)
      (bool-exp (bool) bool)
      (list-exp (lista) lista)
      (vec-exp (vector) vector)
      (reg-exp (registro) registro)
      (expr-bool-exp (expr-bool) expr-bool)
      (else 1))));continuar!!!!