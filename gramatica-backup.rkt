#lang eopl
;Proyecto Final: Fundamentos de Lenguajes de Programación
;
;Desarrolladores:
;;Diana Katherine Toro Ortiz - 2110046
;;Carlos Mauricio Tovar Parra - 1741699
;;Nicolás Jaramillo Mayor - 1840558
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;<programa>      ::= <globales> <expresion>
;<expresion>     ::= <identificador>
;                ::= var {<identificador> = <expresion>}*(,) in <expresion>
;                ::= cons {<identificador> = <expresion>}*(,) in <expresion>
;                ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
;                ::= unic {<identificador> = <expresion>}*(,) in <expresion>
;                ::= <octal>
;                ::= <numero>
;                ::= <caracter>
;                ::= <cadena>
;                ::= <bool>
;                ::= <lista>
;                ::= <vector>
;                ::= <registro>
;                ::= <expr-bool>
;                ::= sequence {<expresion>}+(;)end
;                ::= if <expr-bool> then <expresion> [ else <expresion> ] end
;                ::= cond {"["<expresion><expresion>"]"}* else <expresion>}
;                ::= while <expr-bool> do <expresion> done
;                ::= for <identificador> = <expresion> (to | downto) <expresion> do <expresion> done
;<globales>      ::= {<identificador> = <expresion>}*(,)
;<lista>         ::= [{<expresion>}*(;)]
;<vector>        ::= vector[{<expresion>}*(;)]
;<registro>      ::= {{<identificador> = <expresion>}+(;)}
;<expr-bool>     ::= compare(<expresion><pred-prim><expresion>)
;                ::= <oper-bin-bool>(<expr-bool>, <expr-bool>)
;                ::= <bool>
;                ::= <oper-un-bool>(<expr-bool>)
;<pred-prim>     ::= < | > | <= | >= | == | <>
;<oper-bin-bool> ::= and | or | xor
;<oper-un-bool>  ::= not
;<arit-prim-10>  ::= + | - | * | % | / | ++ | --
;<arit-prim-8>   ::= + | - | *  | ++ | --
;<cad-prim>      ::= longitud | concatenar
;<list-prim>     ::= vacio | vacio? | crear-lista | lista? | cabeza | cola | append
;<vect-prim>     ::= vector? | crear-vector | ref-vector | set-vector
;<reg-prim>      ::= registros? | crear-registro | ref-registro | set-registro

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
    (octal ("x8" "("  digit (separated-list (arbno digit) " ") ")") number)
    (caracter ("'" (letter) "'") symbol)
    (cadena ("\"" (arbno letter) "\"") string)
    (bool ("\"" (or true false) "\"") string)
   )
)




(define gramatica
'(
    (programa (globales expresion) un-programa)
    (globales () glob)
    (expresion (identificador) id-exp)
    (expresion 
        ("var" (separated-list 
            (arbno identificador "=" expresion) ",")
        "in" expresion) 
        var-exp)
    (expresion 
        ("cons" (separated-list 
            (arbno identificador "=" expresion)",")
        "in" expresion)
        cons-exp)
    (expresion ;rec??????????
        ("rec" (arbno 
            identificador 
            (separated-list 
                (arbno identificador) ",")
            "=" expresion)
        "in" expresion)
        rec-exp)
    (expresion 
        ("unic" 
        (separated-list 
            (arbno identificador "=" expresion)","))
        unic-exp)
    (expresion (numero) num-exp)
    (expresion (octal) oct-exp)
    (expresion (caracter) cara-exp)
    (expresion (cadena) cad-exp)
    (expresion (bool) bool-exp)
    ;(expresion (lista) list-exp)
    ;(expresion (vector) vec-exp)
    ;(expresion (registro) reg-exp)
    (expresion (expr-bool) expr-bool-exp)
    (expresion (
        "sequence" expresion ";" 
        (separated-list 
            (arbno expresion) ";")
        "end")
        seq-exp)
    (expresion ("if" expr-bool "then" expresion "[" "else" expresion "]" "end")
    if-exp)
    (expresion ("cond" (arbno "["expresion expresion"]") "else" expresion "end")
    cond-exp)
    (expresion ("while" expr-bool "do" expresion "done") while-exp)
    ;(expresion ("for" identificador "=" expresion (or "to" "downto") expresion "do" expresion "done")
    ;for-exp)
    (expr-bool () ex)


    (expresion ("["(separated-list (arbno expresion) ";")"]") list-exp)
)
)
    
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

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

;(define interpretador
;  (sllgen:make-rep-loop  "--> "
;    (lambda (pgm) (eval-program  pgm)) 
;    (sllgen:make-stream-parser 
;      lexica
;      gramatica)))
