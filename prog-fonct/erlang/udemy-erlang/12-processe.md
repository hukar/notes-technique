# Processus

## thread

Def : fil d'éxécution, tâche, unité de traitement

similaire à un processus

Chaque processus a un espace de mémoire virtuelle qui lui est propre.

Les thread partagent la mémoire virtuelle de leur processus.

Par contre chaque thread a sa propere pile d'éxécution.

Deux processus sont totalement indépendants et isolés l'un de l'autre.

En comparaison, à part leur pile d'éxécution, les threads partagent tout.

### Un processus ne peut pas partager des données

### Un thread peut partager des données

## `spawn`

```erlang
spawn(module, function [list of parameters])
```

```erlang
-module(speak).

-export([run/0,say/2]).
say(What, 0) ->
    done; %% keyword in Erlang
say(What, Times) ->
    io:fwrite(What ++ "\n"),
    say(What, Times - 1).

run() ->
    spawn(speak, say, ["hi", 3]),
    spawn(speak, say, ["Bye", 3]).
```

`done`  mot clé en Erlang pour finir une fonction qui ne renvoie rien.

On doit exporter `say/2`  car `spawn` lance un nouveau processus du module `speak` appelant `say`.

```erlang
4> c(speak). %% compilation
speak.erl:4: Warning: variable 'What' is unused
{ok,speak}
5> speak:run().
hi
Bye
<0.94.0> %% fin du processus de speak:run
hi
Bye
hi
Bye
```

Il y a trois processus lancés en parallèle .