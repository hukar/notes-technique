# 01 Les Basic I Simon Thompson

## Types de données

Les integer sont convertis automatiquement en grand nombre sans problèmes d'overflow.

### Différentes bases `#`  (de 2 à 36 `[0..9][a..z]`)

```erlang
1> 2#1101.
13
1> 8#44.
36
2> 7#23.
17
3> 7#10.
7
4> 17#10.
17
5> 16#abc.
2748
```

### Récupérer la valeur ASCII `$char` 

```erlang
6> $$.
36
7> $A.
65
8> $a.
97
```

### atom

```erlang
atom_1 %% commençant par une lettre minuscule et pouvant contenir des lettres, des entiers et des underscores
aTom_2
%% ou n'importe quel caractère encapsulé par ''
'__123 un atom Biz@rre !!$^'
```

### Référence unique `#ref`

```erlang
make_ref().
#Ref<0.1756049109.2986082309.72377>
```

Génère une référence unique

### Tuples

Les `tuples` ont un nombre fixe d'items, c'est la différence crucial avec les `lists`.

### Lists

Les listes peuvent contenir un nombre variable d'éléments de n'importe quel type.

Les listes ont une taille dynamique.

Les Strings en Erlang sont des listes de valeurs ASCII.

```erlang
15> length([r,t]).
2
16> length([r,[t,u]]).
2

17> [65,66,66,65].
"ABBA" %% les chaines sont des listes de code ASCII
18> "ABBA".
"ABBA"
19> length("ABBA").
4
```

#### Liste propre et impropre

```erlang
25> [45|3]. %% liste impropre le reste n'est pas unbe liste
[45|3]

26> [45|[3|[]]]. %% liste propre
[45,3]

27> [45|[3]]. %% aussi liste propre
[45,3]
```

#### Clear dans le shell erl

```erlang
io:format(os:cmd(clear)).
```

## `++` et `--`

```erlang
28> [[1,2]|[1,2,3]].
[[1,2],1,2,3]

29> [1,2]++[1,2,3]. %% résultat différent qu'avec |
[1,2,1,2,3]
```

```erlang
30> [1,2]--[1,2,3].
[]

31> [1,1,2,3]--[1,2].
[1,3]
```

## Variables

Elles commencent par une capitale ou un `_`.

Elle ne contiennent pas de caractères spéciaux.

`_` tout seul est `don't care variable`, sa valeur est ignorée et elle n'est jamais liée.

#### ! on peut assigner une variable une et une seule fois.

```erlang
33> A=7.
7
34> A.
7
35> A+A.
14
36> A+10.
17
37> A=9. %% on ne peut assigner une variable qu'une fois
** exception error: no match of right hand side value 9
38> f(A). %% libère le nom de variable A (forget)
ok
39> A=9. %% on peut alors le réutiliser
9
40> _=23. %% le joker
23
41> _.
* 1: variable '_' is unbound
42> _=56.
56
```

## Pattern Matching

```
Pattern = Expression
```

1- assigner une variable

2- Contrôle de flux

3- Extraire les données de type de données composées

```erlang
45> {B,C}={5,78}.
{5,78}
46> B.
5
47> C.
78

48> {D,D,E}={5,5,67}. %% D et D matchent deux fois sur le même nombre
{5,5,67}
49> D.
5
50> E.
67

51> {F,F,G}={23,24,68}. %% ici F et F matchent sur deux nombres différents
** exception error: no match of right hand side value {23,24,68}
                               
52> A=8.
8
53> A=9. %% même erreur que plus haut
** exception error: no match of right hand side value 9
54> A=8. %% matche correcte
8

55> M=[2|[34]].
[2,34]
56> [P|Q]=M.
[2,34]
57> P.
2
58> Q. %% Les chaînes sont des tableau de code ASCII
"\""
59> Q=[34]. %% matche correctement
"\""
```

### Extraction de valeur

```erlang
60> {Url,_,_,[Name|_],{_,Size}}={"www.titi.com",popo,titi,["michel",5678],{tintin,167}}.
{"www.titi.com",popo,titi,["michel",5678],{tintin,167}}
61> Url.
"www.titi.com"
62> Name.
"michel"
63> Size.
167
```

## Boolean

Ce sont deux atoms `true` et `false` .