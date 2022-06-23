||02 liste de compréhension

```erlang
[M*4 || M <- [1,2,3]].
[4,8,12]
```

`||` pipe [expression || générateur]

`M <- [a,b,c]`  M prend succéssivement les valeurs de la listes.

## Avec condition(s)

```
[ expression || X <- List, condition, condition, ... ]
```

```erlang
Hwy = [{hw12,45},{hw67,55},{hwy112,65},{hwy125,75}].

[{H,Speed - 5} || {H, Speed} <- Hwy, Speed >= 55].
[{hw67,50},{hwy112,60},{hwy125,70}]
```

Maintenant avec deux conditions :

```erlang
[{H,Speed - 5} || {H, Speed} <- Hwy, Speed >= 55, Speed =< 65].
[{hw67,50},{hwy112,60}]
```

#### ! `>=` et `=<` (pas de flêche)

## Itérer sur plusieurs listes

```erlang
% même nombre d'éléments
9> [A - B || A <- [4,5], B <- [7,9]].
[-3,-5,-2,-4]

% nombre d'éléments différent
10> [A - B || A <- [4,5], B <- [7,9, 2]].
[-3,-5,2,-2,-4,3]
```

Exemple plus gourmand :

```erlang
11> [{A, B} || A <- [celeri, poireau], B <- [boulette, carpaccio, saucisse]].
[{celeri,boulette},
 {celeri,carpaccio},
 {celeri,saucisse},
 {poireau,boulette},
 {poireau,carpaccio},
 {poireau,saucisse}]
```

Exemple avec pattern matching :

```erlang
CityLocation = [{newyork, us},{losangeles, us},{paris, france},{toronto,canada},{vancouver,canada}].
[{newyork,us},
 {losangeles,us},
 {paris,france},
 {toronto,canada},
 {vancouver,canada}]

[City || {City,us} <- CityLocation].
[newyork,losangeles]

[City || {City,france} <- CityLocation].
[paris]

[Countrie || {vancouver,Countrie} <- CityLocation].
[canada]
```

