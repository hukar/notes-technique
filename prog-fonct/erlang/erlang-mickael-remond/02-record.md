# 02 les records

## Définir un record

```erlang
-module(rec).

%% c'est ici que l'on défini le record
-record(robot,{id, name, power, speed, engine, type}).

-export([create_robot/2]).

create_robot(Name, Power) ->
    #robot{
        id = gen_id(),
        name = Name,
        power = Power,
        speed = (Power / 2) + 10*Power + math:sqrt(Power),
        _ = unknowed %% valeur pour les champs non spécifié
    }.

gen_id() ->
    erlang:unique_integer().
```

```erlang
 R = rec:create_robot("Toto", 78).
{robot,-576460752303423293,"Toto",78,827.8317608663278,
       unknowed,unknowed}
```

#### ! Si pas de valeur par default précisée -> undefined

```erlang
create_robot(Name) ->
    #robot{
        name = Name
    }.
```

```erlang
2> rec:create_robot("Michel").
{robot,undefined,"Michel",undefined,undefined,undefined,
       undefined}
```

## Modifier une valeur

```erlang
-record(cat,{name, age}).

modify_cat_name(Cat, Name) ->
    Cat#cat{name = Name}.
```

```erlang
8> OtherCat= rec:modify_cat_name(Cat, "nounouche").
{cat,"nounouche",11}

9> Cat. %% immutabilité des variables
{cat,"minouche",11}
10> OtherCat.
{cat,"nounouche",11}
```

Comme on peut le voire, cette technique ne modifie pas réellement l'enregistrement de départ, mais crée un nouvel entregistrement.

## Accéder à un champs notation `.`

```erlang
get_cat_name(Cat) ->
    Cat#cat.name.
```

### `Variable#record_name.field_name`

## Pattern Matching avec les record

```erlang
display_cat_name(#cat{name = Name}) ->
    io:format("cat name is : ~p~n", [Name]).
```

```erlang
12> rec:display_cat_name(Cat).
cat name is : "minouche"
ok
```

### Traitement conditionnel d'un record grâce au Pattern Matching

```erlang
kill_old_cat(#cat{name = Name, age = 11}) ->
    io:format("I kill this cat ~p~n", [Name]);
kill_old_cat(#cat{name = Name}) ->
    io:format("meouw ~p is too young to die~n", [Name]).
```

```erlang
6> NewCat.
{cat,"minouche_new",7}

7> Cat.
{cat,"minouche",11}

8> rec:kill_old_cat(Cat). %% match avec age = 11
I kill this cat "minouche"
ok
9> rec:kill_old_cat(NewCat).
meouw "minouche_new" is too young to die
ok
```

