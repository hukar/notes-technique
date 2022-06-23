# 06 Sequential III

## Runtime Errors: match

### function clause

Erreur de matching avec les fonctions :

```erlang
% error.erl
-module(error).

-export([fact/1]).


fact(N) when N > 0 -> N * fact(N - 1);
fact(0) -> 1.
```

```erlang
c(error).
{ok,error}

error:fact(-1).
** exception error: no function clause matching 
                    error:fact(-1) (error.erl, line 6)
```

### case clause

Erreur avec `case` :

```erlang
% toujours dans error.erl
test(N) ->
    case N of
        -1 -> false;
        1 -> true
    end.
```

```erlang
 c(error).
{ok,error}

error:test(6).
** exception error: no case clause matching 6
     in function  error:test/1 (error.erl, line 10)
```

### if clause

Erreur avec `if` :

```erlang
% toujours dans error.erl
testif(N) ->
    if
        N < 0 -> false;
        N > 0 -> true
    end.
```

```erlang
error:testif(0).
** exception error: no true branch found when evaluating an if expression
     in function  error:testif/1 (error.erl, line 16)
```

### badmatch

Erreur de pattern matching :

```erlang
T = {1, two, 3}.
{1,two,3}

{1, two, 3, four} = T.
** exception error: no match of right hand side value {1,two,3}
```

## Runtime Errors: Others

## badarg

Est renvoyé lorsqu'une fonction du langage ( BIF Built In function) reçoie un mauvais argument.

```erlang
f(). % dé-lie toutes les variables liées (forget)
ok

T={my_tuple}.
{my_tuple}

length(T).
** exception error: bad argument in function  length/1 
		called as length({my_tuple})
```

## Undef : undefined function

Si la fonction appelée n'est pas définie ou exportée.

```erlang
error:hello().
** exception error: undefined function error:hello/0
```

## badarith

quand une opération arithmétique est exécutée avec autre chose qu'un nombre :

```erlang
6 + two.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as 6 + two
```

mais aussi pour un calcul impossible **division par zéro**  :

```erlang
56 / 0.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  '/'/2
        called as 56 / 0
```

## catch and Throw

```
catch <expression>
```

il attrape les sortie de programme des erreurs à l'exécution.

il renvoie le tuple `{'EXIT', Reason}`

```erlang
testif(N) ->
    if
        N < 0 -> false;
        N > 0 -> true
    end.

catchif(N) -> catch testif(N).
```

```erlang
error:catchif(0).
{'EXIT',{if_clause,[{error,testif,1,
                           [{file,"error.erl"},{line,16}]},
                    {error,catchif,1, 
                           [{file,"error.erl"},{line,21}]},
                    {erl_eval,do_apply,6,
                              [{file,"erl_eval.erl"},
                               {line,684}]},
                    {shell,exprs,7,
                           [{file,"shell.erl"},{line,686}]},
                    {shell,eval_exprs,7,
                           [{file,"shell.erl"},{line,642}]},
                    {shell,eval_loop,3,
                           [{file,"shell.erl"},{line,627}]}]}}
```

### Si on attrape pas (catch) une exception, le processus est tué (killed)

```erlang
self(). % renvoie l'ID du processus
<0.116.0>

catch Nb = 5 / 0.
{'EXIT',{badarith,[{erlang,'/',[5,0],[]},
                   {erl_eval,do_apply,6,
                             [{file,"erl_eval.erl"},{line,684}]},
                   {erl_eval,expr,5,
                             [{file,"erl_eval.erl"},{line,449}]},
                   {erl_eval,expr,5,
                             [{file,"erl_eval.erl"},{line,437}]},
                   {shell,exprs,7,
                          [{file,"shell.erl"},{line,686}]},
                   {shell,eval_exprs,7,
                          [{file,"shell.erl"},{line,642}]},
                   {shell,eval_loop,3,
                          [{file,"shell.erl"},{line,627}]}]}}

self().
<0.116.0> % le processus est le même

Pipi = 8 + titi.
** exception error: an error occurred when evaluating an arithmetic expression
     in operator  +/2
        called as 8 + titi
                                             
self().
<0.128.0> % le processus a été tué et relancé par le shell
```

### `self()`  : Id du processus

### Syntax du catch

```erlang
catch 1/0.
{'EXIT',{badarith,[{erlang,'/',[1,0],[]},
                   {erl_eval,do_apply,6,
                             [{file,"erl_eval.erl"},{line,684}]},
                   {erl_eval,expr,5,
                             [{file,"erl_eval.erl"},{line,437}]},
                   {shell,exprs,7,
                          [{file,"shell.erl"},{line,686}]},
                   {shell,eval_exprs,7,
                          [{file,"shell.erl"},{line,642}]},
                   {shell,eval_loop,3,
                          [{file,"shell.erl"},{line,627}]}]}}

X = catch 1/0. % le catch ne fonctionne pas
* 1: syntax error before: 'catch'

X = (catch 1/0). % il faut des parenthèses
{'EXIT',{badarith,[{erlang,'/',[1,0],[]},
                   {erl_eval,do_apply,6,
                             [{file,"erl_eval.erl"},{line,684}]},
                   {erl_eval,expr,5,
                             [{file,"erl_eval.erl"},{line,437}]},
                   {erl_eval,expr,5,
                             [{file,"erl_eval.erl"},{line,449}]},
                   {shell,exprs,7,
                          [{file,"shell.erl"},{line,686}]},
                   {shell,eval_exprs,7,
                          [{file,"shell.erl"},{line,642}]},
                   {shell,eval_loop,3,
                          [{file,"shell.erl"},{line,627}]}]}}
```

```
X = (catch <expression>)
```

Il faut des parenthèses.

## throw

#### ! Celà rend le code difficile à débuter et à comprendre

```
throw(<expression>)
```

```erlang
% error.erl
admission(Person) ->
    case Person of
        nana -> throw({alert,{non_grata_person, Person}});
        tutu -> throw({alert,{non_grata_person, Person}});
        _ -> {ok,{guest, Person}}
    end.
```

```erlang
error:admission(titi).
{ok,{guest,titi}}
error:admission(nana).
** exception throw: {alert,{non_grata_person,nana}}
     in function  error:admission/1 (error.erl, line 25)
```

### Un autre exemple

```erlang
integerAddition(X, Y) ->
    testInteger(Y),
    testInteger(Y),
    X + Y.

testInteger(X) when is_integer(X) -> ok;
testInteger(X) -> throw({error, {non_integer, X}}).
```

```erlang
catch error:integerAddition(5, two).
{error,{non_integer,two}}

catch throw(hello_kitty).
hello_kitty

catch error:integerAddition(5,6).
11

throw(hello_koto).
** exception throw: hello_koto
```

