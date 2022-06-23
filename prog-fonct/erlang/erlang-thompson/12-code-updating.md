# 12 Mise à jour du code

Code de départ `app.erl`

```erlang
-module(app).

-export([s/1, c/2, l/0]).

s(Name) ->
    register(Name, spawn(?MODULE, l, [])).

c(Name, X) ->
    Name ! {self(), X},
    receive Y -> Y end.

l() ->
    receive
      	%% on appel l avec son nom pleinement qualifié app:l() 
        update -> app:l();
        {Pid, X} -> Pid ! X + X, l()
    end.
```

```erlang
1> c(app).
{ok,app}

2> {app:s(server1), app:s(server2)}.
{true,true}

3> {app:c(server1, 1), app:c(server2, 1)}.
{2,2}
```

Maintenant on change le code du calcul :

```erlang
%%...
        {Pid, X} -> Pid ! X * X, l()
```

On update que le serveur 1

```erlang
4> c(app), server1 ! update.
update

5> {app:c(server1, 1), app:c(server2, 1)}.
{1,2}
```

Chaque serveur est maintenant sur une version du code différente.

## deux versions du code maximum

On ne peut avoir que deux versions du code en simultanée :

```erlang
1> c(app).
{ok,app}

2> {app:s(server1), app:s(server2)}.
{true,true}

3> {app:c(server1, 3), app:c(server2, 3)}.
{6,6}
```

Maintenant on change l'opération en `*` :

```erlang
4> c(app), server2 ! update.
update

5> {app:c(server1, 3), app:c(server2, 3)}.
{6,9}
```

Tout fonctionne, on a une version 1 décode pour `server1` et un version 2 pour `server2`.

```js
6> c(app), server1 ! update.
```

On compile de nouveau le code avec `X * X * X` , on a donc un troisième version du code qui vient supprimer la version 1.

Il ne reste que la version 2 et 3 du code.

`server1` qui été sur la version 1 n'existe plus :

```
** exception error: bad argument
     in operator  !/2
        called as server1 ! update
```

`server2`lui existe toujours et peut être mis à jour :

```erlang
7> server2 ! update.
update

8> app:c(server2, 3).
27
```

## Résumé

![Capture d’écran 2019-06-17 à 11.32.31](assets/Capture d’écran 2019-06-17 à 11.32.31.png)

Deux versions du module peuvent tournées ensemble à l'exécution.![Capture d’écran 2019-06-17 à 11.33.33](assets/Capture d’écran 2019-06-17 à 11.33.33.png)

Il faut qu'une fonction (pleinement qualifié = préfixé par le nom de son module) soit appelé pour que le processus pointe vers le nouveau code.

![Capture d’écran 2019-06-17 à 11.34.50](assets/Capture d’écran 2019-06-17 à 11.34.50.png)

Il ne peut y avoir que deux versions du code en même temps.

Si un processus pointe une plus vieille version du code, il est tué.

## Gestion des versions du code

Utilisation du `code path`

`code:get_path()`  : 

- le repertoire courant

- Tous les répertoire dans \`$ERLANGROOT/lib directory

On peut ajouter des répertoire au début ou à la fin du `code path` avec :

`code:add_patha(Dir)`  au début de la liste

`code:add_pathz` à la fin.

```erlang
11> code:get_path().
[".",
 "/usr/local/Cellar/erlang/22.0.2/lib/erlang/lib/kernel-6.4/ebin",
 "/usr/local/Cellar/erlang/22.0.2/lib/erlang/lib/stdlib-3.9.1/ebin",
 [...]
```

```erlang
12> code:add_patha("/Users/kms/Documents/programmation/erlang-thompson/sequential").
true
13> code:get_path().                                                                
["/Users/kms/Documents/programmation/erlang-thompson/sequential",
 ".",
 "/usr/local/Cellar/erlang/22.0.2/lib/erlang/lib/kernel-6.4/ebin",
 "/usr/local/Cellar/erlang/22.0.2/lib/erlang/lib/stdlib-3.9.1/ebin",
 [...]
```

J'ajoute au `path` un répertoire à moi au début.

## Linker une librairie

`Lib/mylib.erl`

```erlang
-module(mylib).

-export([f/1]).

f(X) ->
    X + 2.
```

Dans son module on se réfère à cette librairie :

```erlang
-module(applib).

-export([g/1]).

g(Y) ->
		%% ici on se réfère à la librairie mylib
    mylib:f(Y) + 8.
```

On doit ajouter le chemin au path dans le shell erlang :

```erlang
4> code:add_patha("/Users/kms/Documents/programmation/erlang-thompson/sequential").
true
```

```erlang
5> c(applib).
{ok,applib}
6> applib:g(1).
11
```

### Supprimer manuellement une version du code `purge`   `soft_purge`

`code:purge(Module)` détruit le module même si un processus pointe sur ce code.

`code:soft_purge(Module)` ne détruit le module que si aucun processus ne pointe vers le code de ce module 

```
8> c(app), code:soft_purge(app).
true
9> app:s(server).
true
```

? je ne comprends pas l'utilisation de `soft_purge` ?

