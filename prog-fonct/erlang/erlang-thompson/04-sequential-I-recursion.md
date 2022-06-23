# 04 Sequential I récursivité

## calculs récursifs

#### rappel `div`division entière `/`division virgule flotante

```erlang
-module(calc).

-export([average/1]).

average(X) -> sum(X) div len(X).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

len([_|T]) -> 1 + len(T); % utilisation de _ car on utilise pas la valeur
len([]) -> 0.
```

Autre exemple :

```erlang
-module(print).

-export([printAll/1]).

printAll([]) ->
    io:format("~n", []);
printAll([X|Xs]) ->
    io:format("~p ", [X]),
    printAll(Xs).
```

```
9> print:printAll([1,2,3,4]).       
1 2 3 4 
```

On peut ré-écrire la fonction avec un case

```erlang
-module(print).

-export([printAll/1]).

printAll(Ys) ->
    case Ys of
        [] ->
            io:format("~n", []);
        [X|Xs] ->
            io:format("~p ", [X]),
            printAll(Xs)
    end.
```

`io:format()` ecrit dans la sortie standard.

Ce procédé s'appel *tail recursive function* , c'est le dernier appel, la dernière ligne de la clause.

Il ne produit pas de pile, mais écrit sur le dernier appel.

## Quelques fonctions récursives

```erlang
double([H|T]) -> [2*H|double(T)]; % map les éléments
double([]) -> [].

even([H|T]) when H rem 2 == 0 -> % filtre les éléments
    [H|even(T)];
even([_|T]) -> even(T); % H est impaire
even([]) -> [].

member(H, [H|_]) -> true; % utilisation pattern matching
member(H, [_|T]) -> member(H, T);
member(_, []) -> false.
```

On peut voire que `_` est souvent utilisé.

Dans les deux premiers cas on construit une nouvelle liste de manière récursive. 

## Tail Recursion = Récursivité terminal

Version de `sum` avec un accumulateur :

```erlang
sum2([], S) -> S;
sum2([H|T], S) -> 
    sum2(T, S + H).
```

```
31> calc:sum2([5,6,7,8],0).
26
```

C'est une *tail recursive function* (la dernière expression évalué est un appel récursif).

### Nouvelle version average en tail recursion

```erlang
av(X) -> average2(X, 0, 0).

average2([], S, L) -> S div L;
average2([H|T], S, L) -> average2(T, S + H, L + 1). % la dernière expression est juste un appel récursif
```

```
35> calc:av([11,19,18,2,5,17,11]).
11
```

On utilise deux accumulateurs pour le calcul.

## Fibonacci

normal :

```erlang
fib(0) -> 0;
fib(1) -> 1;
fib(I) ->
    fib(I -1) + fib(I - 2).
```

```
40> calc:fib(13).
233
```

Recursive terminal :

```erlang

```

## La boucle for

```erlang
-module(forloop).

-export([for/3]).

for(Max, Max, F) ->
    [F(Max)];
for(I, Max, F) ->
    [F(I)|for(I + 1, Max, F)].
```

```erlang
forloop:for(1, 5, fun(X) -> X*3 + 2 end).
[5,8,11,14,17]
```

