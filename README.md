### C-vid
![Alt text](/relative/path/to/logo_cvid.png?raw=true "C-vid")
Lenguaje de programación C-vid, desarrollado para el curso Fundamentos de Lenguajes de programación de la Universidad del Valle

# Gramática del lenguaje:

```
<programa>      ::= <globales> <expresion>
<globales>      ::= "("{<identificador> = <expresion>}")"*(,)
<expresion>     ::= <identificador>
                ::= var {<identificador> = <expresion>}*(,) in <expresion>
                ::= cons {<identificador> = <expresion>}*(,) in <expresion>
                ::= rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion>
                ::= unic {<identificador> = <expresion>}*(,) in <expresion>
                ::= <octal>
                ::= <numero>
                ::= <caracter>
                ::= <cadena>
                ::= <lista>
                ::= <vector>
                ::= <registro>
                ::= <expr-bool>
                ::= sequence {<expresion>}+(;)end
                ::= if <expr-bool> then <expresion> [ else <expresion> ] end
                ::= cond {"["<expresion><expresion>"]"}* else <expresion>}
                ::= while <exp-bool> do <expresion> done
                ::= for <identificador> = <expresion> (to | downto) <expresion> do <expresion> done
<expr-bool>     ::= compare(<expresion><pred-prim><expresion>)
                ::= <oper-bin-bool>(<expr-bool>, <expr-bool>)
                ::= <bool>
                ::= <oper-un-bool>(<expr-bool>)
<identificador> ::= <letter> | {<letter> | 0,...,9}*
<letter>        ::= A...Z | a...z
<numero>        ::= [-]{0,...,9}* | [-]{0,...,9}* . {0,...,9}*
<octal>         ::= x8 "("{0,...,7}+( )")"
<caracter>      ::= (')<letter>(')
<cadena>        ::= ("){<caracter>}*(")
<bool>          ::= true | false
<lista>         ::= [{<expresion>}*(;)]
<vector>        ::= vector[{<expresion>}*(;)]
<registro>      ::= {{<identificador> = <expresion>}+(;)}
<pred-prim>     ::= < | > | <= | >= | == | <>
<oper-bin-bool> ::= and | or | xor
<oper-un-bool>  ::= not
<arit-prim-10>  ::= + | - | * | % | / | ++ | --
<arit-prim-8>   ::= + | - | *  | ++ | --
<cad-prim>      ::= longitud | concatenar
<list-prim>     ::= vacio | vacio? | crear-lista | lista? | cabeza | cola | append
<vect-prim>     ::= vector? | crear-vector | ref-vector | set-vector
<reg-prim>      ::= registros? | crear-registro | ref-registro | set-registro
```