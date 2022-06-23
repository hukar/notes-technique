# Les maps

Les maps sont un ensemble de données sous la forme clé/valeur.

```erlang
#{key=>value}
```

## Fonction avec un map

```erlang
-module(age).

-export([getAge/1]).

getAge(Name) ->
    AgeMap = #{"Alice" => 23, "Bob" => 25, "Carol" => 29},
    maps:get(Name, AgeMap, -1).
```

```erlang
9> age:getAge("tonton").
-1 %% ma valeur par défaut en troisième argument

10> age:getAge("Alice").
23 %% il retrouve bien l'age d'Alice
```

`maps:get(Key, MyMap [, DefaultValue])`  La valeur par défaut est optionnel.

On a donc deux arité différentes pour `maps:get` : `maps:get/2` et `maps:get/3`

