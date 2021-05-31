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

;<programa>      ::= <globales> <expresion>
;<globales>      ::= "("{<identificador> = <expresion>}")"*(,)
;<expresion>     ::= <identificador>
;                ::= var {<identificador> = <expresion>}*(,) in <expresion>
;                ::= sta {<identificador> = <expresion>}*(,) in <expresion>
;                ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;                ::= unic {<identificador> = <expresion>}*(,) in <expresion>
;                ::= <octal>
;                ::= <numero>
;                ::= <caracter>
;                ::= <cadena>
;                ::= <lista>
;                ::= <vector>
;                ::= <registro>
;                ::= <expr-bool>
;                ::= sequence {<expresion>}+(;)end
;                ::= if "(" <expr-bool> ")" then <expresion> [ else <expresion> ] end
;                ::= cond {"["<expresion><expresion>"]"}* else <expresion>}
;                ::= while "(" <exp-bool> ")" do <expresion> done
;                ::= for "(" <identificador> = <expresion> ")" (to | downto) <expresion> do <expresion> done
;<expr-bool>     ::= compare(<expresion><pred-prim><expresion>)
;                ::= <oper-bin-bool>(<expr-bool>, <expr-bool>)
;                ::= <bool>
;                ::= <oper-un-bool>(<expr-bool>)
;<identificador> ::= <letter> | {<letter> | 0,...,9}*
;<letter>        ::= A...Z | a...z
;<numero>        ::= [-]{0,...,9}* | [-]{0,...,9}* . {0,...,9}*
;<octal>         ::= x8 "("{0,...,7}+( )")"
;<caracter>      ::= (')<letter>(')
;<cadena>        ::= ("){<caracter>}*(")
;<bool>          ::= true | false
;<lista>         ::= [{<expresion>}*(;)]
;<vector>        ::= vector[{<expresion>}*(;)]
;<registro>      ::= {{<identificador> = <expresion>}+(;)}
;<pred-prim>     ::= < | > | <= | >= | == | <>
;<oper-bin-bool> ::= and | or | xor
;<oper-un-bool>  ::= not
;<arit-prim>     ::= + | - | * | % | / | ++ | --
;<cad-prim>      ::= longitud | concatenar
;<list-prim>     ::= vacio | vacio? | crear-lista | lista? | cabeza | cola | append
;<vect-prim>     ::= vector? | crear-vector | ref-vector | set-vector
;<reg-prim>      ::= registros? | crear-registro | ref-registro | set-registro

;expresiones adicionales 
;<expresion>     ::= ":" "(" <expresion> <arit-prim> <expresion> ")"
;                ::= <cad-prim> <cadena>
;                ::= <list-prim> <lista>
;                ::= <vect-prim> <vector>
;                ::= <reg-prim> <registro>
;                ::= if "(" <list-prim> <lista> ")" then <expresion> [ else <expresion> ] end       
;                ::= define <cadena> "lambda" "(" {<expresion>}* ")" <expresion>   
  
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
    (octal ("x8("(arbno(or "0""1""2""3""4""5""6""7")) ")") string)
    (caracter ("´"(or digit letter)) symbol)
    (cadena ("\"" (arbno (or letter digit whitespace)) "\"") string)
    (bool ((or "true" "false")) string)
    (hasta ((or "to" "downto")) string)
   )
)

;Especificación Sintáctica (gramática)

(define gramatica
  '(
    (programa (globales expresion) un-programa)    
    (globales ("global" "(" (separated-list identificador "=" expresion ",") ")" ) glob-exp)

    ;producciones de tipo expresión
    (expresion (identificador) id-exp)
    (expresion ("var" "("(separated-list identificador "=" expresion ",")")" "in" expresion) var-exp)
    (expresion ("sta""("(separated-list identificador "=" expresion ",")")" "in" expresion) sta-exp)
    (expresion ("rec" (arbno identificador "("(separated-list identificador ",")")" "=" expresion) "in" expresion) rec-exp)
    (expresion ("@value") c-vid-val-exp)
    (expresion ("unic" "("(separated-list identificador "=" expresion ",")")" "in" expresion) unic-exp)
    (expresion (octal) oct-exp)
    (expresion (numero) num-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (lista) list-exp)
    (expresion (vector) vec-exp)
    (expresion (registro) reg-exp)
    (expresion (expr-bool) boolean-exp)
    (expresion ("sequence" "(" expresion ";" (arbno expresion ";") ")" "end") seq-exp)
    (expresion ("if" "(" expr-bool ")" "then" expresion "[" "else" expresion "]" "end") if-exp)
    (expresion ("cond" (arbno "["expresion expresion"]") "else" expresion "end") cond-exp)
    (expresion ("while" "(" expr-bool ")" "do" expresion "done") while-exp)
    (expresion ("for" "(" identificador "=" expresion hasta expresion ")" "do" expresion "done") for-to-exp)

    ;expresiones adicionales 
    (expresion (":" "(" expresion arit-prim expresion ")") oper-exp)
    (expresion ("o:" "(" expresion arit-prim-octal expresion ")") oper-exp-oct)
    (expresion (cad-prim) pred-cadena)
    (expresion (list-prim "(" lista ")") pred-list)
    (expresion (vect-prim) pred-vector)
    (expresion (reg-prim) pred-registro)
    (expresion ("define" identificador "lambda" "("(arbno expresion)")" expresion) funcion)
    (expresion ("call" identificador "("(arbno expresion)")") call-funcion)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;primitivas de cadenas
    (cad-prim ("longitud"  "("cadena")") cadena-long)
    (cad-prim ("concatenar" "("(separated-list cadena ",")")") cadena-con)
    
    ;---------- LISTAS -----------
    (lista ("["(separated-list expresion ";")"]") list)
    (lista ("vacia") empt-list)
    (lista ("cons" "("expresion lista")") cons-list)
    (lista ("append" "("lista lista")") append-list)
    ;(lista (identificador) id-list)   ???
    
    ;primitivas de listas
    (expr-bool ("lista?"  lista ) lista-pred)
    (expr-bool ("vacio?"  lista ) lista-vacia-pred)
    (list-prim ("cabeza") lista-cabeza)
    (list-prim ("cola") lista-cola)

    ;---------- VECTORES -----------
    (vector ("vec" "["(separated-list expresion ";")"]") vec)
    
    ;primitivas de vectores
    (expr-bool ("vector?" "("vector")") vector-pred)
    (vect-prim ("ref-vector" "("numero "de" vector")") vector-ref)
    (vect-prim ("set-vector" "("expresion "en" numero "de" vector")") vector-set)

    ;---------- REGISTROS -----------
    (registro ( "{" identificador ":" expresion  (arbno "," identificador ":" expresion) "}" ) regist)

    ;primitivas de registros
    (expr-bool ("registro?" "("registro")") registro-pred)
    (reg-prim ("ref-registro" "("identificador "de" registro")") registro-ref)
    (reg-prim ("set-registro" "("expresion "en" identificador "de" registro")") registro-set)

    ;---------- BOOLEANOS -----------
    ;expresiones booleanas
    (expr-bool (bool) bool-exp)
    (expr-bool ("compare" "(" expresion pred-prim expresion ")" ) bool-comp-exp)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") bool-oper-exp)
    (expr-bool ("not" "(" expr-bool ")") not-bool-exp)

    ;operadores booleanos
    (oper-bin-bool ("and") and)
    (oper-bin-bool ("or") or)
    (oper-bin-bool ("xor") xor)
    
    ;predicado de primitivas
    (pred-prim ("<") menor)
    (pred-prim (">") mayor)
    (pred-prim ("<=") menor-igual)
    (pred-prim (">=") mayor-igual)
    (pred-prim ("==") igual)
    (pred-prim ("<>") entre)

    ;---------- ARITMETICA -----------

    ;primitivas aritmeticas para decimales
    (arit-prim ("+") suma)
    (arit-prim ("-") resta)
    (arit-prim ("*") multiplicacion)
    (arit-prim ("/") division)
    (arit-prim ("++") aumentar)    
    (arit-prim ("--") disminuir)

    ;primitivas aritmeticas para octales
    (arit-prim-octal ("+") suma-octal)
    (arit-prim-octal ("-") resta-octal)
    (arit-prim-octal ("*") multiplicacion-octal)
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

;******************************************************************************************

