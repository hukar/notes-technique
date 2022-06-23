# 01 Introduction au livre de mickael Redmond

Erlang :

- Approche concurrente
- approche déclarative (prolog)



Concept de noeud erlang



`Toolbar:start()` remplacé par `observer:start()` ou `debugger:start()`

## `calendar:is_leap_year`  == année bissextile

```erlang
1> calendar:is_leap_year(2000).
true
2> calendar:is_leap_year(2020).
true
```

## `calendar:local_time()`

```erlang
3> calendar:local_time().
{{2019,6,11},{10,25,27}}
```

## `calendar:last_day_of_the_month`

```erlang
4> calendar:last_day_of_the_month(2016,06).
30
5> calendar:last_day_of_the_month(2020,02).
29
```

## Terminer l'interpréteur

```erlang
init:stop()
```

## Langage déclaratif

On décrit le quoi != langage impératif : on décrit le comment.

Déclaratif == Fonctionnel

## Composition

f(g(X))

g(f(X))

```erlang
9> X = 2.
2

11> calc:add8(calc:mul3(X)).
14
12> calc:mul3(calc:add8(X)).
30
```

## Quick Sort

```erlang
quick_sort([]) ->
  []; %% une liste vide retourne une liste vide
quick_sort([Median|Tail]) ->
  quick_sort([N || N <- Tail, N < Median]) ++ [Median] ++ quick_sort([N || N <- Tail, N >= Median]).

%% quick_sort(Petit) ++ La médiane ++ quick_sort(Grand)
```

## Caractère

obtenir le code ASCII :

```erlang
$a.
97
```

## Écriture exponentielle

```erlang
14> 12345.0 == 1.2345e4.
true
15> 12345.0 == 1.2345E4.
true
```

Avec `e` majuscule ou minuscule

## Afficher le nombre de décimales

`~X.Yf`

`X` : nombre total de chiffre `.` séparateur inclus

`Y` : nombre dérrière le séparateur

```erlang
X=345.678543217889. 

22> io:format("~6.2f~n",[X]).
345.68
ok
23> io:format("~.2f~n",[X]). 
345.68
```

## Atomes

mémoire de stockage non récupérée par le ramasse miette.

#### ! ne pas générer les atomes de manière dynamique

## Nombre aléatoire = `rand:uniform(value)`

```erlang
tenRandomNumbers(D) ->
    throwDice(D, 10).

throwDice(N, 0) ->
    finish;
throwDice(N, Acc) ->
    io:format("~p~n",[rand:uniform(N)]),
    throwDice(N, Acc - 1).
```

```
38> calc:tenRandomNumbers(6).
1
5
4
6
6
1
1
3
2
4
finish
```

Donne un entier aléatoire de `1` à `Value`

## Lister les processus : i().

```
39> i().
Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               erl_init:start/2                       376     1964    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0              1598    33148    0
erts_code_purger      erts_code_purger:wait_for_request        1              
<0.2.0>               erts_literal_area_collector:start      233    32475    0
...
```

## Types composés

listes => données non ordonnées

tuples => données ordonnées



### Extraction d'éléments

```erlang
41> [A, B, C| Tail] = [1, 2, 3, 4, 5, 6].
[1,2,3,4,5,6]
42> A.
1
43> B.
2
44> C.
3
45> Tail.
[4,5,6]
```

On peut extraire une ou plusieurs valeurs avec l'écriture `[Val1, val2, …, Valn | Tail]`

