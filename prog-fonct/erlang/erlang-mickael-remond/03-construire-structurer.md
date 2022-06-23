# 03 Construire et structurer

#### en-tête

##### -expression(Terme, [Terme]).

#### Corps

##### functions

## `-export`

Interface publique

`-import`existe aussi mais est à éviter.

Un module sans export peut compiler, mais n'a pas de sens.

## Créer une macro `-define`

`foo.hrl`

```erlang
-define(TITI, "Mr Titi").

-define(INDICE, 1.5689).
```

On crée un fichier `.hrl` qui ne contient que des en-têtes .

`bar.erl`

```erlang
-module(bar).

-include("foo.hrl"). %% on inclus ici notre fichier d'en-tête

-export([display/0]).

display() ->
    io:format("titi : ~p, indice ~p~n", [?TITI,?INDICE]). %% on utilise une macro avec le sign ?
```

```erlang
18> bar:display().
titi : "Mr Titi", indice 1.5689
ok
```

`-define` pour définir une macro.

```erlang
-define(NOM_MACRO, "valeur").
```

`?` Pour utiliser la macro.

```erlang
?NOM_MACRO
```

### `-include`

Pour inclure le fichier d'en-tête on utilise :

```erlang
-include("path/file.hrl").
```

