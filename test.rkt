#lang eopl
(require "./gramatica.rkt")
(require "./interprete.rkt")


;Pruebas 

;programa con globales y una expresion
(scan&parse "global () x")                                      ;id-exp
(scan&parse "global () &x")                                     ;ref-id-exp
(scan&parse "global (ix = 2, cc =23, s=2) var (x=2,z=34) in e") ;var-exp
(scan&parse "global (ix = 2) sta (a=1,b=2) in e")               ;sta-exp
(scan&parse "global () rec x (s,d,f,g) = e1 in e2")             ;rec-exp
(scan&parse "global () unic (a=1,b=2) in e")                    ;unic-exp
(scan&parse "global () unic (a=1,b=@value) in e")               ;c-vid-val-exp
(scan&parse "global () x8(12)")                                 ;oct-exp
(scan&parse "global () 3")                                      ;num-exp
(scan&parse "global () ´s")                                     ;cara-exp
(scan&parse "global () \"a 3hola 3foo bar    mundo   3e  \"")   ;cad-exp
(scan&parse "global () [2;3;4;5;4;3;5;a;d;v;z]")                ;list-exp
(scan&parse "global () vec[2;3;4;5;4;3;5;a;d;v;z]")             ;vec-exp
(scan&parse "global () { a:3, v:5}")                            ;reg-exp
(scan&parse "global () false")                                  ;boolean-exp (bool-exp)
(scan&parse "global () compare(3>2)")                           ;boolean-exp (bool-comp-exp)
(scan&parse "global () and(true,false)")                        ;boolean-exp (bool-oper-exp)
(scan&parse "global () not(false)")                             ;boolean-exp (not-bool-exp)
(scan&parse "global () sequence (a;s;3;d;) end")                ;seq-exp
(scan&parse "global () if (compare(2>5)) then a [else b] end")  ;if-exp
(scan&parse "global () cond [compare(2>5) c] else b end")       ;cond-exp
(scan&parse "global () while (compare(2>5)) do e done")         ;while-exp
(scan&parse "global () for (a=1 to 10) do e done")              ;for-to-exp
(scan&parse "global () :(1 + 10)")                              ;oper-exp
(scan&parse "global () o:(x8(123) + x8(321))")                  ;oper-exp-oct
(scan&parse "global () longitud (\"a 3hola 3fomundo   3e  \")") ;pred-cadena (cadena-long)
(scan&parse "global () longitud (idcadena)")                    ;pred-cadena (cadena-long)
(scan&parse "global () concatenar (\"foo  \", \"bar\", \"2\")") ;pred-cadena (cadena-con)
(scan&parse "global () concatenar (idcadena)")                  ;pred-cadena (cadena-con)
(scan&parse "global () set x=3")                                ;set-exp
(scan&parse "global (x = 5, y = 3) var (z = 4) in sequence ({x : z, z : 9};) end ")   ;uso de variables y globales
;lists
(scan&parse "global () [1;2;3;4;5;6]")                          ;list-exp (list)
(scan&parse "global () cons(4 cons(3 [2;1;3]))")                ;list-exp (cons-list)
(scan&parse "global () cons(4 cons(3 idlista))")                ;list-exp (cons-list)
(scan&parse "global () append ( [1;2;3] [4;5;6] )")             ;list-exp (append-list)
(scan&parse "global () append ( [1;2;3] idlista )")             ;list-exp (append-list)
(scan&parse "global () lista? ([1;2;3;4;5;6])")                   ;lista-pred
(scan&parse "global () lista? (idlista)")                         ;lista-pred
(scan&parse "global () vacia")                                  ;empt-list
(scan&parse "global () vacia? []")                              ;lista-vacia-pred
(scan&parse "global () vacia? idlista")                         ;lista-vacia-pred
(scan&parse "global () cabeza([1;2;3])")                        ;pred-list (lista-cabeza) [list]
(scan&parse "global () cabeza(idlista)")                        ;pred-list (lista-cabeza) [list]
(scan&parse "global () cabeza(cons(4 cons(3 vacia)))")          ;pred-list (lista-cabeza) [cons-list]
(scan&parse "global () cola  (cons(4 cons(3 vacia)))")          ;pred-list (lista-cola)
(scan&parse "global () cola  (cons(4 cons(3 idlista)))")        ;pred-list (lista-cola)
;vectors
(scan&parse "global () vec[1;2;3]")                             ;vec-exp (vec)
(scan&parse "global () vector? (vec[1;2;3])")                   ;vector-pred
(scan&parse "global () vector? (idvector)")                     ;vector-pred
(scan&parse "global () ref-vector (3 de vec[3;3;4])")           ;pred-vector (vector-ref)
(scan&parse "global () ref-vector (3 de idvector)")             ;pred-vector (vector-ref)
(scan&parse "global () set-vector (2 en 3 de vec[3;3;4])")      ;pred-vector (vector-set)
(scan&parse "global () set-vector (2 en 3 de idvector)")        ;pred-vector (vector-set)
;registros
(scan&parse "global () {xc:2}")                                 ;reg-exp (regist)
(scan&parse "global () registro? ({xc:2})")                     ;registro-pred
(scan&parse "global () registro? (idregistro)")                 ;registro-pred
(scan&parse "global () ref-registro (xc de {xc:2})")            ;pred-registro (registro-ref)
(scan&parse "global () ref-registro (xc de idregistro)")        ;pred-registro (registro-ref)
(scan&parse "global () set-registro (123 en xc de {xc:2})")     ;pred-registro (registro-set)
(scan&parse "global () set-registro (123 en xc de idregistro)") ;pred-registro (registro-set)
;boolean
(scan&parse "global () true")                                   ;boolean-exp (bool-exp)
(scan&parse "global () compare(3<2)")                           ;boolean-exp (bool-comp-exp) [menor]
(scan&parse "global () compare(3>2)")                           ;boolean-exp (bool-comp-exp) [mayor]
(scan&parse "global () compare(3<=2)")                          ;boolean-exp (bool-comp-exp) [menor-igual]
(scan&parse "global () compare(3>=2)")                          ;boolean-exp (bool-comp-exp) [mayor-igual]
(scan&parse "global () compare(3==2)")                          ;boolean-exp (bool-comp-exp) [igual]
(scan&parse "global () compare(3<>2)")                          ;boolean-exp (bool-comp-exp) [entre]
(scan&parse "global () and(true,false)")                        ;boolean-exp (bool-oper-exp) [and]
(scan&parse "global () or(true,false)")                         ;boolean-exp (bool-oper-exp) [or]
(scan&parse "global () xor(true,false)")                        ;boolean-exp (bool-oper-exp) [xor]
(scan&parse "global () not(false)")                             ;boolean-exp (not-bool-exp)
;aritmetica
(scan&parse "global () :(1 + 10)")                              ;oper-exp (suma)
(scan&parse "global () :(1 - 10)")                              ;oper-exp (resta)
(scan&parse "global () :(1 * 10)")                              ;oper-exp (multiplicacion)
(scan&parse "global () :(1 / 10)")                              ;oper-exp (division)
(scan&parse "global () :(1++)")                               ;oper-exp (aumentar)
(scan&parse "global () :(1--)")                               ;oper-exp (disminuir)

;octales
(scan&parse "global () o:(x8(123) + x8(132))")                  ;oper-exp-oct (suma-octal)
(scan&parse "global () o:(x8(123) - x8(132))")                  ;oper-exp-oct (resta-octal)
(scan&parse "global () o:(x8(123) * x8(132))")                  ;oper-exp-oct (multiplicacion-octal)
(scan&parse "global () o:(x8(123) ++)")                         ;oper-exp-oct (aumentar-octal)
(scan&parse "global () o:(x8(123) --)")                          ;oper-exp-oct (disminuir-octal)

;pruebas con funciones
;función predicado
(scan&parse
 "global ()
    define filtro
      lambda (lista pred)
         if (vacio? (lista))
         then [vacia]
         [else
            if (¿pred cabeza(lista))
            then [cons(cabeza(lista) call filtro(cola(lista) pred))]
            [else call filtro(cola(lista) pred)]
            end]
         end")

;funcion factorial

(scan&parse "global () call factorial (:(n - 1))") ;call-funcion
(scan&parse          ;funcion
 "global ()
    define factorial
      lambda (n)
         cond
             [compare (n == 0) 1]
             [compare (n == 1) 1]
             else :(n * call factorial (:(n - 1)))
             end")

(scan&parse          ;funcion
 "global ()
    rec factorial (n) = cond
             [compare (n == 0) 1]
             [compare (n == 1) 1]
             else :(n * call factorial (:(n - 1)))
             end
        in (factorial 5)")


; Pruebas en el intérprete
;(ejecutar en el intérprete)

;rec a (x, y) = :(3+4) in call(a 1 2)
;rec fact (x) = if (compare (x > 0)) then :(x * call(fact :(x --))) [else 1] end in call(fact 6)
;rec a (x, y) = :(x+y) b (f, j) = :(f+j) in :(call(a 1 2) + call(b 3 4))

;[]
;[1]
;[1;2;3]
;vacia
;cons (1 [2;3;4])
;append ([1;2][2;3;5;6])
;append ([1;2] append([2;3][5;6]))
;¿lista? (append ([1;2] cons(2 [5;6])))
;¿vacio? (vacia)
;¿vacio? ([2])
;cabeza (append ([1;2] cons(2 [5;6])))
;cola (append ([1;2] cons(2 [5;6])))

;vec[2;3]
;¿vect?(vec[2])
;¿vect?(38)
;ref-vect(2 de vec[0;1;2;3;4])
;global (x = vec[0;1;2;3;4]) set-vect(99 en 2 de x) x
;ref-reg(a de {a:2})
;ref-reg(a de {a:3,b:4,c:77})
;set-reg(12 en a de {a:3,b:4,c:77})
;global (x = {a:3,b:4,c:77}) set-reg(12 en b de x) x