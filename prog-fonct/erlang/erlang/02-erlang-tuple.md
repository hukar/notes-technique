# 02 erlang Tuple

Les tuples sont des données en nombre connue et regroupées.

````erlang
X =4.   
4
Y = 8.
8
Point = {X,Y}.
{4,8}
````

On peut directement écrire `Point = {5,8}`.

## Pattern Matching

Erlang regarde les variables non-liée à gauche (*unbound*) et leurs affecte les valeurs à droite afin que les deux côtés du égal correspondent.

````erlang
Point = {45,56}.
{45,56}
{Titi, Grominet} = Point.
{45,56}
Titi.
45
````

### the anonymous _ variable / la variable anonyme : `_`

````erlang
Perso = {michel,dupont,46}.
{michel,dupont,46}
{Nom,_,Age} = Perso.
{michel,dupont,46}
Nom.
michel
Age.
46
````

Le **pattern matching**g fonctionnent seulement si il y a le même nombre d'éléments des deux côtés :

````erlang
MonChien = {wolfi,7,{doudou,nonos}}.
{wolfi,7,{doudou,nonos}}
{_,{_,NomDoudou}}=MonChien.
** exception error: no match of right hand side value {wolfi,7,{doudou,nonos}}
                                 
{_,_,{_,NomDoudou}} = MonChien.
{wolfi,7,{doudou,nonos}}
NomDoudou.
nonos

````

Un tuple contenant un `atom` s'appelle un `tagged tuple`

### Les tuples peuvent contenir toutes sortes de types différents

