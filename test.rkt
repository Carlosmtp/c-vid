#lang eopl
(require "./gramatica.rkt")


;Pruebas

;programa con globales y una expresion
(scan&parse "global () x")                                      ;id-exp
(scan&parse "global (ix = 2, cc =23, s=2) var (x=2,z=34) in e") ;var-exp
(scan&parse "global (ix = 2) sta (a=1,b=2) in e")               ;sta-exp
(scan&parse "global () rec x (s,d,f,g) = e1 in e2")             ;rec-exp
(scan&parse "global () unic (a=1,b=2) in e")                    ;unic-exp
(scan&parse "global () unic (a=1,b=@value) in e")
(scan&parse "global () x8(12)")                                 ;oct-exp
(scan&parse "global () 3")                                      ;num-exp
(scan&parse "global () ´s")                                     ;cara-exp
(scan&parse "global () \"a 3hola 3foo bar    mundo   3e  \"")   ;cad-exp
(scan&parse "global () [2;3;4;5;4;3;5;a;d;v;z]")                ;list-exp
(scan&parse "global () vector[2;3;4;5;4;3;5;a;d;v;z]")          ;vec-exp
(scan&parse "global () { a:3, v:5}")                             ;reg-exp
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
(scan&parse "global () o:(x8(123) + x8(321))")                  ;oper-exp-oct
(scan&parse "global (x = 5, y = 3) var (z = 4) in sequence ({x : z, z : 9};) end ")   ;uso de variables y globales

(scan&parse "global () cons(4 cons(3 [2;1;3]))")                ;cons-list
(scan&parse "global () set x=3")
(scan&parse "global () vacia")                                  ;empt-list
(scan&parse "global () cabeza([1;2;3])") 
(scan&parse "global () cabeza(cons(4 cons(3 vacia)))")
(scan&parse "global () vector? (vector[1;2;3])")
(scan&parse "global () ref-vector (3 de vector[3;3;4])")
(scan&parse "global () set-vector (2 en 3 de vector[3;3;4])")

(scan&parse "global () append ( [1;2;3] [4;5;6] )")
(scan&parse "global () registro? ({xc:2})")
(scan&parse "global () ref-registro (x de {xc:2})")
(scan&parse "global () set-registro (123 en x de {xc:2})")

;pruebas con funciones

;función predicado
(scan&parse
   "global ()
    define predicado
      lambda (lista pred)
         if (vacio? ([1;2;3]))
         then []
         [else
            if (lista? ([1;2;3]))
            then []
            [else y]
            end]
         end")

;funcion factorial
(scan&parse
   "global ()
    define factorial
      lambda (n)
         cond
             [compare (n == 0) 1]
             [compare (n == 1) 1]
             else :(n * 1)
             end")
