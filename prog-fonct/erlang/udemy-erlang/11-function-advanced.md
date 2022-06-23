# Les fonctions avancées

## Pattern Matching

```erlang
22> {kiki, Nb} = {kiki, 78}.
{kiki,78}
23> Nb.
78
```

Le Pattern Matching permet de faire corespondre la partie gauche et la partie droite d'une égalité.

## Gards

`when` ajoute une clause au Pattern Matching.

```erlang
-module(age2).

-export([getType/1]).

getType(N) when N < 13 -> %% when ajoute une condition
    child;
getType(N) when N < 18 ->
    teenager;
getType(N) -> 
    adult.
```

```erlang
3> age2:getType(5).
child
4> age2:getType(15).
teenager
5> age2:getType(150).
adult
```

## Built In

```erlang
%% round
2> round(5.5).
6
3> round(5.4).
5

%% trunc
4> trunc(5.5).
5
5> trunc(5.9).
5

%% length
6> length([1,2,3,4,5]). 
5

%% float
10> float(5).
5.0

%% is_atom
11> is_atom(hello).
true
12> is_atom("hello").
false

%% is_tuple
13> is_tuple({a,g}).
true
14> is_tuple([1, 2]).
false

%% atom_to_list
15> atom_to_list(atom).
"atom"
```

#### ! List et String c'est la même chose en Erlang

## Higher Order Function

Fonction de classe supérieur = de première classe

C'est une fonction qu'on peut assigner à une variable;

Wikipedia :

> - elles prennent une ou plusieurs fonctions en entrée ;
> - elles renvoient une fonction.

Voire objet de première classe

> - être expressible comme une valeur anonyme littérale ;
> - être affecté à des [variables](https://fr.wikipedia.org/wiki/Variable) ou des [structures de données](https://fr.wikipedia.org/wiki/Structures_de_donn%C3%A9es) ;
> - avoir une identité intrinsèque ;
> - être comparable pour l'égalité ou l'identité avec d'autres entités ;
> - pouvoir être passé comme paramètre à une procédure ou une fonction ;
> - pouvoir être retourné par une procédure ou une fonction ;
> - pouvoir être constructible lors de l'[exécution](https://fr.wikipedia.org/wiki/Moteur_d%27ex%C3%A9cution).

```erlang
-module(hof).

-export([double/0]).

double() ->
     F = fun(X) -> 2 * X end,
     myMap(F, [1, 23, 6, 37]).

myMap(F, []) ->
    [];
myMap(F, [First | Rest]) ->
    [F(First) | myMap(F, Rest)]. %% | sert ici à joindre
```

Autre exemple personnel :

```erlang
-module(hof).

-export([calculate/2]).

calculate(FC, []) ->
    [];
calculate(FC, [First|Rest]) ->
    [FC(First) | calculate(FC, Rest)]. %% création d'un tableau
```

```erlang
11> AddFive = fun(X) -> X + 5 end.
#Fun<erl_eval.6.128620087>
12> hof:calculate(AddFive, [2,3,4,7]).
[7,8,9,12]
```

