# Hello kitty

```erlang
-module(hk).
-author("kms").

%% API
-export([hellokitty/0]).

hellokitty() ->
  "hello kitty".
```

On crée une fonction et on l'exporte.

Dans le terminal, on lance le shell Erlang:

```bash
$ erl

Eshell V10.3.2  (abort with ^G)
1> c(hk).
{ok,hk}
2> hk:hellokitty().
"hello kitty"
```

`c(hk)`  pour **c**ompiler le fichier `hk`

`hk:hellokitty()`

`Module:fonction()`

## Commentaire du code

`1 fichier` représente un `module`

`-module(nom_du_module).`  Un module doit avoir un nom.

En Erlang on finit souvent avec un point (fin d'un module, fin d'une fonction)

`%`  commentaires

`-export([])` rend accéssible des fonctions à l'extérieur du module.

```erlang
-export([hellokitty/0]).

% ma super fonction
kitty() ->
  "hello kitty".

hellokitty() ->
  kitty().
```

```bash
1> c(hk).
{ok,hk}
2> hk:hellokitty().
"hello kitty"
4> hk:kitty().

```

## Overload (surcharger) une fonction

L'arité d'une fonction est son nombre d'arguments.

#### ! Il faut une majuscule au début d'une variable

```erlang
-module(hk).

-export([hellokitty/0,hellokitty/1]).
kitty() ->
    "hello kitty".

hellokitty() ->
    kitty().

hellokitty(X) -> %% ici il faut un X majuscule sinon il y a une erreur ** exception error: no function clause matching 
    "hello "++X.
```

```bash
6> c(hk).
{ok,hk}
7> hk:hellokitty("john").
"hello john"
```

