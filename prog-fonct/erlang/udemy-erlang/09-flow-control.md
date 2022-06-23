# Les controles de flux

## If Else

Une fonction pour savoir si une année est bissextile.

```erlang
-module(cal).

-export([leap/1]).

leap(Year) ->
    if
        Year rem 400 == 0 -> leap;  %% si l'année est divisible par 400
        Year rem 100 == 0 -> non_leap; 
        Year rem 4 == 0 -> leap;
        true -> non_leap %% pas de point virgule ici
    end.
```

```erlang
14> cal:leap(2019).
non_leap
15> cal:leap(2000).
leap
```

## Case

```erlang
-module(cal).

-export([leap/1, day/2]).

leap(Year) ->
    if
        Year rem 400 == 0 -> leap;
        Year rem 100 == 0 -> non_leap;
        Year rem 4 == 0 -> leap;
        true -> non_leap %% pas de point virgule ici
    end.

day(Month, Year) ->

    Leap = leap(Year),      

   case Month of
       jan -> 31;
       feb when Leap == leap -> 29; %% gards
       feb -> 28;
       mar -> 31;
       apr -> 30;
       may -> 31;
       jun -> 30;
       jul -> 31;
       aug -> 31;
       sep -> 30;
       oct -> 31;
       nov -> 30;
       dec -> 31 %% pas de point virgule
    end.
```

```erlang
18> cal:day(feb, 2000).
29
19> cal:day(feb, 2019).
28
20> cal:day(sep, 2019).
30
```

## Loops

Pour créer une loop (boucle) en Erlang, on utilise la récursivité :

```erlang
-module(greet).

-export([greet/1]).
greet([]) -> %% condition de fin de boucle
    true;
greet([First|Rest]) ->
    io:fwrite("hello " ++ First ++ "\n"),  %% écrire dans le terminal
    greet(Rest).
```

```erlang
21> c(greet).                                   
{ok,greet}
22> greet:greet(["hello","coco","titi","toto"]).
hello hello
hello coco
hello titi
hello toto
true
```

`io:fwrite("string")`  pour écrire dans le terminal.