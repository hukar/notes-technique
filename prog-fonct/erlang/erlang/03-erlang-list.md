# 03 Erlang List

Les chaîne de caractères sont en fait des listes de caractères :

````erlang
[97,98,97].
"aba"
[233].
"é"
````

Erlang écrit des chiffres si au moins un des chiffres ne correspond pas à un caractère :

````erlang
[99,98,97,7].
[99,98,97,7]
````

## ++ et --

On peut ajouter ou soustraire des listes entre elles :

````erlang
[1,2,3]++[4,5].
[1,2,3,4,5]
[1,6,1,5,1]--[1].
[6,1,5,1]
[1,6,1,5,1]--[1,1].
[6,5,1]
[1,6,1,5,1]--[7].  
[1,6,1,5,1]
````

Ces opérations sont associative à droite :

````erlang
[1,2,3]--[1,2]--[3].
[3]
% ([1,2,3]--([1,2]--[3]))
% ([1,2,3]--[1,2])
% [3]
````

Les `builts in function` BIF, sont souvent implémentée en c, soit car c'était impossible en Erlang, soit pour des raisons de performances. Par exemple la BIF `length(List)` :

````erlang
length([1,2,3,4]).
4
````



## Head et Tail

La tête et la queue.

On peut utiliser les BIF `hd(L)` et `tl(L)` , mais aussi le pipe `|` qui s'appelle le *cons* (*constructor*).

Manipuler le premier élément est rapide avec une liste.

````erlang
L=[1,2,3,4].
[1,2,3,4]
hd(L).
1
tl(L).
[2,3,4]
% utilisation de l'opérateur cons |
NewList=[99|L].
[99,1,2,3,4]
````

Autre exemple :

````erlang
[Tete|Queue]=NewList.
[99,1,2,3,4]
Tete.
99
Queue.
[1,2,3,4]
````

````erlang
[1|[]].
[1]
[1|[2|[]]].
[1,2]
% ...
````

On peut définir une liste de manière récursive avec la formule :

````erlang
[Term1|[Term2|[...|[termN|[]]]]]
````

On peut mettre plusieurs valeurs dans le *head* :

````erlang
L=[1,2].
[1,2]
[3,4,5|L].
[3,4,5,1,2]
````

Une dernière écriture dite impropre :

````erlang
LI=[1|2].
[1|2]
% du coup length() ne fonctionne pas avec une liste impropre !!
length(LI).
** exception error: bad argument
     in function  length/1
        called as length([1|2])
````

L'écriture propre serait :

````erlang
LI=[1|[2]]
````

