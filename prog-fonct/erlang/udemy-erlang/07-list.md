# 07 Les listes

Les listes supportent les opérations d'ajout et de supression d'élément.

```erlang
3> []. % liste vide
[]

4> [1, 2, 3, 4].
[1,2,3,4]

5> [{celsius, 25}, {farenheit, 78}]. % liste de tuples
[{celsius,25},{farenheit,78}]

6> [1, "dodo"]. % liste hétéroclite
[1,"dodo"]
```

## L'opérateur pipe `|`

```erlang
7> [First | Rest] = [titi, 4, "toto"]. %% opérateur pipe |
[titi,4,"toto"]
8> First.
titi
9> Rest.
[4,"toto"] %% c'est une liste

%% avec plusieurs valeur
14> [A, B, C | R] = [a, b, c, d, e, f].
[a,b,c,d,e,f]
15> A.
a
16> B.
b
17> C.
c
18> R.
[d,e,f] %% c'est une liste

19> [V1, V2 | R1] = [1, 2].
[1,2]
20> R1.
[] %% liste vide

%% si il y a plus d'élément à gauche une erreur est levée
[V1, V2, V3 | R1] = [1, 2].
** exception error: no match of right hand side value [1,2]
```

## Attention à la réaffectation des variables

En Erlang une variable ne peut être affectée qu'une fois par **scope**.

```erlang
1> [A, B | R] = [1, 2, 3, 4]. %% première affectation
[1,2,3,4]
2> A.
1
3> R.
[3,4]

4> [C|R] = [a, b, c, d]. %% ré-affectation de R
** exception error: no match of right hand side value [a,b,c,d]
```

## Built-in function length

```erlang
5> L = [r,t,u,i].
[r,t,u,i]
6> length(L). %% donne le nombre d'éléments
4
```

