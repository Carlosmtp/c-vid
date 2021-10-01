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

    ;producciones de tipo expresión
    (expresion
     ("global" "("
               (separated-list identificador "=" expresion ",")
               ")" expresion) 
             glob-list-exp) ;unparse hecho
    (expresion (identificador) id-exp) ;unparse hecho
    (expresion ("&"identificador) ref-id-exp) ;unparse hecho
    (expresion ("var" "("
                (separated-list identificador "=" expresion ",")
                      ")" "in" expresion)
               var-exp) ;unparse hecho
    (expresion ("sta""("(separated-list identificador "=" expresion ",")")" "in" expresion) sta-exp)
    (expresion ("rec" (arbno identificador "("(separated-list identificador ",")")" "=" expresion) "in" expresion) rec-exp) ;Verificar si esta bien
    (expresion ("@value") c-vid-val-exp)
    (expresion ("unic" "("(separated-list identificador "=" expresion ",")")" "in" expresion) unic-exp)
    (expresion ("(x8" (arbno numero) ")") oct-exp) ;unparse hecho
    (expresion (numero) num-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (lista) list-exp)
    (expresion (vect) vec-exp)
    (expresion (registro) reg-exp)
    (expresion (expr-bool) boolean-exp)
    (expresion ("sequence" "(" expresion ";" (arbno expresion ";") ")" "end") seq-exp) ;verificar si esta bien
    (expresion ("if" "(" expresion ")" "then" expresion "[" "else" expresion "]" "end") if-exp) ;Verificar si esta bien
    (expresion ("cond" (arbno "["expresion expresion"]") "else" expresion "end") cond-exp)
    (expresion ("while" "(" expresion ")" "do" expresion "done") while-exp)
    (expresion ("to") to-exp)
    (expresion ("downto") downto-exp)
    (expresion ("for" "(" identificador "=" expresion expresion expresion ")" "do" expresion "done") for-to-exp)

    ;expresiones adicionales 
    (expresion (cad-prim) pred-cadena)
    (expresion (list-prim "(" expresion ")") pred-list)
    (expresion (vect-prim) pred-vect)
    (expresion (reg-prim) pred-registro)
    (expresion ("define" identificador "lambda" "("(arbno expresion)")" expresion) funcion)
    (expresion ("set" identificador "=" expresion) set-exp) ;unparse hecho
    (expresion ( "-(" expresion (arbno expresion) ")")  app-exp);Verificar si esta bien

    ;primitivas de cadenas
    (cad-prim ("longitud"  "("expresion")") cadena-long)
    (cad-prim ("concatenar" "("(separated-list expresion ",")")") cadena-con)
    
    ;---------- LISTAS -----------
    (lista ("["(separated-list expresion ";")"]") list-e)
    (lista ("vacia") empt-list)
    (lista ("cons" "("expresion expresion")") cons-list)
    (lista ("append" "("expresion expresion")") append-list)
    
    ;primitivas de listas
    (expr-bool ("lista?" "("expresion")" ) lista-pred)
    (expr-bool ("vacio?" "("expresion")" ) lista-vacia-pred)
    (list-prim ("cabeza") lista-cabeza)
    (list-prim ("cola") lista-cola)

    ;---------- VECTORES -----------
    (vect ("vec" "["(separated-list expresion ";")"]") vec)
    
    ;primitivas de vectores
    (expr-bool ("vect?" "("expresion")") vect-pred)
    (vect-prim ("ref-vect" "("numero "de" expresion")") vect-ref)
    (vect-prim ("set-vect" "("expresion "en" numero "de" expresion")") vect-set)

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
    (expresion (":" "(" expresion arit-prim")") oper-exp)
    
    ;primitivas aritmeticas para decimales
    (arit-prim ("+" expresion) suma)
    (arit-prim ("~" expresion) resta)
    (arit-prim ("*" expresion) multiplicacion)
    (arit-prim ("/" expresion) division)
    (arit-prim ("++") aumentar)    
    (arit-prim ("--") disminuir)

    (expresion ("o:" "(" expresion arit-prim-octal")") oper-exp-oct)
    ;primitivas aritmeticas para octales
    (arit-prim-octal ("+" expresion) suma-octal)
    (arit-prim-octal ("-" expresion) resta-octal)
    (arit-prim-octal ("*" expresion) multiplicacion-octal)
    (arit-prim-octal ("++") aumentar-octal)    
    (arit-prim-octal ("--") disminuir-octal)
 )
)
;o:(x8(1)+ x8(2))
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
