# 03 Erlang Sequentiel I

## Case expression

```
Case expression of
	case clause (pattern) -> ;
	case clause;
	__ no separator in the last clause
end
```

```erlang
case lists:member(foo, List) of
	true -> ok;
	false -> {error,unknown}
end          
```

Pas de séparateur pour la dernière clause

### utilisation d'un wild card `_` pour avoir une clause par défaut.

#### ! Philosophie d'Erlang: ne programmez pas de manière défensive, laisser le programme crasher (let's it fail!)

### Premièrement programmation défensive

```erlang
-module(day).

-export([convert/1]).

convert(Day) ->
    case Day of
        monday -> 1;
        tuesday -> 2;
        wednesday -> 3;
        thursday -> 4;
        friday -> 5;
        saturday -> 6;
        sunday -> 7;
        Other -> {error,unknown_day} % on traite les cas non voulus
        %_-> {error, unknow_day} avec un wildcard
    end.
```

```erlang
2> day:convert(monday).
1
3> day:convert(october).
{error,unknown_day}
```

### Deuxièmement Philosophie Erlang : on laisse crasher

```erlang
-module(day).

-export([convert/1]).

convert(Day) ->
    case Day of
        monday -> 1;
        tuesday -> 2;
        wednesday -> 3;
        thursday -> 4;
        friday -> 5;
        saturday -> 6;
        sunday -> 7 % pas de séparateur pour la dernière clause
    end.
```

```erlang
5> day:convert(tuesday).
2
6> day:convert(november).
** exception error: no case clause matching november
     in function  day:convert/1 (day.erl, line 6)
```

Une exception est levée par Erlang.

## Le if

```
							if								
					 {		X < 1 -> smaller;
if clauses {		X > 1 -> greater;
					 { 		X == 1 -> equal
					 		end
					 				|
					 			guards
```

le If comporte plusieurs clauses, chacune composé d'un guards et d'une expresion (ou plusieurs), séparé par un pont-virgule.

Pas de point-virgule pour la dernière clause.

### Générer un nombre aléatoire

```erlang$
rand:uniform(Nb).
```

```erlang
17> rand:uniform(8).
4
18> rand:uniform(8).
5
19> rand:uniform(8).
7
```

#### Générer un booléen

```erlang
23> (rand:uniform(16) > 8).
true
24> (rand:uniform(16) > 8).
false
25> (rand:uniform(16) > 8).
false
```

### Structure générale

```erlang
if
	Guard1->
		expr,
		expr,
		...
		expr;
	Guard2->
		expr,
		expr,
		...
		expr;
	true-> % le guard true n'est pas obligatoire
    expr,
    expr,
    ...
    expr
end
```

## Guards

Le mot clé `when` introduit un `guard`.

```erlang
-module(fact).

-export([factorial1/1,factorial2/1]).

factorial1(0) -> 1; % sans guard
factorial1(N) ->
    N*factorial1(N-1).

factorial2(N) when N > 0 ->  % avec guard l'ordre des clauses s'inversse
    N * factorial2(N-1);
factorial2(0) -> 1.
```

Exemple avec une fonction de conversion :

```erlang
-module(conv).

-export([convert/1]).

convert(String) when is_list(String) ->
    list_to_atom(String);
convert(Atom) when is_atom(Atom) ->
    Atom.
```

```erlang
4> conv:convert("tututut").
tututut % convertit en atom
5> conv:convert(tutu).
tutu % reste en atom
```

### Autres built-in utilisable dans les `guards`

```erlang
6> is_number(5.6).
true

8> is_integer(5.6).
false

9> is_float(5.6).
true

% is_...
is_atom(X)
is_pid(X)
is_tuple(X)
is_list(X)

% length
length(list) == Int
tuple_size(tuple) == size

% comparaison
X == Y
X =/= Y % X différent de Y
X =:= Y % X exactement égal à Y

10> X=5.0.
5.0
11> Y=5.
5
12> X==Y.
true
13> X=:=Y.
false

% inégalité
X =< Y
X >= Y
% et pas X => Y et X <= Y
```

## Recherche avec `if` et avec `guard`

```erlang
% utilisation du if
linear([Head|Rest], X) ->   
    if 
        Head == X ->
            {finded, Head};
        Rest == [] -> 
            -1;
        true -> % true pour faire le else
            linear(Rest, X) % pas de point virgule
    end.

% utilisation seulement d'un when et du pattern matching
linear2([], X) ->
    -1;
linear2([Head|Rest], X) when Head == X ->
    {finded, X};
linear2([Head|Rest], X) ->
    linear2(Rest, X).
```

## `and` -> `,` et `or` -> `;`

```erlang
factorial2(N) when is_integer(N), N > 0 -> % la virgule séparant les deux conditions vaut pour un et logique
    N * factorial2(N-1);
factorial2(0) -> 1.
```

Plusieurs condition différentes :

```erlang
-module(grd).

-export([multi/1]).

multi(X) when is_tuple(X); is_atom(X); is_integer(X), X > 0  ->
    "clause one";
multi(X) ->
    "clause two".
```

```
27> grd:multi(5).
"clause one"
28> grd:multi({meme,toto}).
"clause one"
29> grd:multi(toto).       
"clause one"
30> grd:multi(5.9). 
"clause two"
```

## Toute fonction n'est pas admise dans un guard

```erlang
f() ->
    (rand:uniform(16) > 8). % renvoie un booléen

if
	f() -> ok; % illegal guard expression
	true -> error
end.
```

Il faut écrire :

```erlang
case f() of
	true -> ok;
	false -> error
end.
```

