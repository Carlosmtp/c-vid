#lang eopl
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
;                ::= cons {<identificador> = <expresion>}*(,) in <expresion>
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
    (for ((or "to" "downto")) string)
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
    (expresion ("cons""("(separated-list identificador "=" expresion ",")")" "in" expresion) cons-exp)
    (expresion ("rec" (arbno identificador "("(separated-list identificador ",")")" "=" expresion) "in" expresion) rec-exp)
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
    (expresion ("for" "(" identificador "=" expresion for expresion ")" "do" expresion "done") for-to-exp)

    ;expresiones adicionales 
    (expresion (":" "(" expresion arit-prim expresion ")") oper-exp)
    (expresion (cad-prim cadena) pred-cadena)
    (expresion (list-prim lista) pred-list)
    (expresion (vect-prim vector) pred-vector)
    (expresion (reg-prim registro) pred-registro)
    (expresion ("if-pred" "(" list-prim lista ")" "then" expresion "[" "else" expresion "]" "end") if)
    (expresion ("define" cadena "lambda" "("(arbno expresion)")" expresion) funcion)
    
    ;lista-vector-registro
    (lista ("["(separated-list expresion ";")"]") list)
    (vector ("vector" "["(separated-list expresion ";")"]") vec)
    (registro ( "(" identificador "=" expresion  (arbno ";" identificador "=" expresion) ")" ) regist)

    ;expresiones booleanas
    (expr-bool ("compare" "(" expresion pred-prim expresion ")" ) bool-comp-exp)
    (expr-bool (oper-bin-bool "(" expr-bool "," expr-bool ")") bool-oper-exp)
    (expr-bool (bool) bool-exp)
    (expr-bool (oper-un-bool "(" expr-bool ")") not-bool-exp)

    ;predicado de primitivas
    (pred-prim ("<") menor)
    (pred-prim (">") mayor)
    (pred-prim ("<=") menor-igual)
    (pred-prim (">=") mayor-igual)
    (pred-prim ("==") igual)
    (pred-prim ("<>") entre)

    ;operadores booleanos
    (oper-bin-bool ("and") and)
    (oper-bin-bool ("or") or)
    (oper-bin-bool ("xor") xor)
    (oper-un-bool ("not") not)

    ;primitivas aritmeticas
    (arit-prim ("+") suma)
    (arit-prim ("-") resta)
    (arit-prim ("*") multiplicacion)
    (arit-prim ("/") division)
    (arit-prim ("++") aumentar)    
    (arit-prim ("--") disminuir)

    ;primitivas de cadenas
    (cad-prim ("longitud") cadena-long)
    (cad-prim ("concatenar") cadena-con)

    ;primitivas de listas
    (list-prim ("vacio") lista-vacia)
    (list-prim ("vacio?") lista-vacia-pred)
    (list-prim ("crear-lista") lista-crear)
    (list-prim ("lista?") lista-pred)
    (list-prim ("cabeza") lista-cabeza)
    (list-prim ("cola") lista-cola)    
    (list-prim ("append") lista-append)

    ;primitivas de vectores
    (vect-prim ("vector?") vector-pred)
    (vect-prim ("crear-vector") vector-crear)
    (vect-prim ("ref-vector") vector-ref)
    (vect-prim ("set-vector") vector-set)

    ;primitivas de registros
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

;******************************************************************************************

;Pruebas

;programa con globales y una expresion
(scan&parse "global () x")                                      ;id-exp
(scan&parse "global (ix = 2, cc =23, s=2) var (x=2,z=34) in e") ;var-exp
(scan&parse "global (ix = 2) cons (a=1,b=2) in e")              ;cons-exp
(scan&parse "global () rec x (s,d,f,g) = e1 in e2")             ;rec-exp
(scan&parse "global () unic (a=1,b=2) in e")                    ;unic-exp
(scan&parse "global () x8(12)")                                 ;oct-exp
(scan&parse "global () 3")                                      ;num-exp
(scan&parse "global () ´s")                                     ;cara-exp
(scan&parse "global () \"a 3hola 3foo bar    mundo   3e  \"")   ;cad-exp
(scan&parse "global () [2;3;4;5;4;3;5;a;d;v;z]")                ;list-exp
(scan&parse "global () vector[2;3;4;5;4;3;5;a;d;v;z]")          ;vec-exp
(scan&parse "global () (a =3;v=5)")                             ;reg-exp
(scan&parse "global () compare(3>2)")                           ;boolean-exp (bool-comp-exp)
(scan&parse "global () and(true,false)")                        ;boolean-exp (bool-oper-exp)
(scan&parse "global () false")                                  ;boolean-exp (bool-exp)
(scan&parse "global () not(false)")                             ;boolean-exp (not-bool-exp)
(scan&parse "global () sequence (a;s;3;d;) end")                  ;seq-exp
(scan&parse "global () if (compare(2>5)) then a [else b] end")  ;if-exp
(scan&parse "global () cond [compare(2>5) c] else b end")       ;cond-exp
(scan&parse "global () while (compare(2>5)) do e done")         ;while-exp
(scan&parse "global () for (a=1 to 10) do e done")              ;for-to-exp
(scan&parse "global () :(1 + 10)")                              ;oper-exp

(scan&parse "global(x = 5, y = 3) var (z = 4) in sequence ((x = z; z = 9);) end ")   ;uso de variables y globales

;pruebas con funciones

;función predicado
(scan&parse
   "global ()
    define \"predicado\"
      lambda (lista pred)
         if-pred (vacio? [1;2;3])
         then []
         [else
            if-pred (lista? [1;2;3])
            then []
            [else y]
            end]
         end")

;funcion factorial
(scan&parse
   "global ()
    define \"factorial\"
      lambda (n)
         cond
             [compare (n == 0) 1]
             [compare (n == 1) 1]
             else :(n * 1)
             end")

