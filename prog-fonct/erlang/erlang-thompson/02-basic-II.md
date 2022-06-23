# Les basic II

## Les fonctions

la tête : le nom de la fonction  appliqué à un pattern:

```erlang
area({square, Side}) %% la tête = the head
```

Les clauses : les différentes têtes d'un même nom de fonction :

```erlang
area({square,Side}) ->
  Side*Side;
area({circle,Radius}) ->
  3.14*Radius*Radius;
area({triangle,A,B,C}) ->
  S=(A+B+C)/2, %% formule de héron
  math:sqrt(S*(S-A)*(S-B)*(S-C));
area(Other) ->
  {error,invalid_object} .
```

Ici il y a quatre clauses (quatre cas).

`,` virgule=coma sépare les expressions dans une clause.

`;` point-virgule=semicolon sépare les clauses

`.` Point finit la fonction.

`:` double-point=colon pour appeler une fonction appartenant à un module module:fonction(Arg, … ) 

## Arrité

Nombre d'argument d'une fonction

```erlang
talk(Word) %% talk/1
talk(Word, Verb) %% talk/2
```

##L'ordre de matching

```erlang
f(A,A) -> A+1;
f(A,B) -> A+B.

f(2,2) %% 3
f(3,4) %% 7
```

#### ! l'importance de l'ordre des clauses :

```erlang
f(A,B) -> A+B;
f(A,A) -> A+1. %% n'est jamais matché !!!!

f(2,2) %% 4
f(3,4) %% 7
```

## Factoriel

```erlang
factorial(0) -> 1;
factorial(N) -> N*factorial(N-1).
```

## Module

Le nom du module et du fichier doivent être les mêmes.

`foo.erl`

```erlang
-module(foo).

-export([]).
% un commentaire
```

## Shell commandes

```erlang
help() % affiche la liste des commandes disponnibles
h() % - history - les 20 dernières commandes
b() % - binding - montre les variables liées
f() % - forget - libère tout les noms de variables liés
f(X) % libère le nom X (désassigne)
e(N) %  - evaluate - evalue la n:th commande
e(-1) % évalue la dernière commande
q() % - quit - 
```

