# 11 Process error handling II

-  Système robuste

- Serveur robuste

## En construisant le système en couche, nous le rendons robuste

Le niveau n-1 capture et corrige les erreurs survenu au niveau n

## Dans un système bien conçu, le programmeur n'a pas à gérer les erreurs

la gestion d'erreur est un isolé à un plus haut niveau du système

Elle est gérée de la même manière dans tous les processus

## les processus dont la seule tâche est de superviser les enfants sont appelé supervisors

![Capture d’écran 2019-05-24 à 16.33.23](assets/Capture d’écran 2019-05-24 à 16.33.23.png)

Les processus du haut supervise les processus d'un niveau plus bas.

## Utilisation de `link` : B crash -> A crash

**Serveur :**

```erlang
-module(app).

-export([start/0, init/0]).

%% start init loop

start() ->
    spawn(app, init, []).

init() ->
    Books = {[], ["alice in wonderland", "bilbo the hobbit", "black dhalia"]},
    loop(Books).

loop(Books) ->
    receive
        {Pid, borrow} ->
            {NewBooks, Book} = take_book(Books, Pid),
            %% link(Pid),
            Pid ! Book,
            loop(NewBooks);
        stop ->
            ok
    end.

take_book({BorrowedBooks, []}, _) ->
    {{BorrowedBooks,[]}, no_book_available};

take_book({BorrowedBooks, [Book|FreeBooks]}, Pid) ->
    {{[{Book, Pid}| BorrowedBooks],FreeBooks}, Book}.

```

Le serveur propose des livres en prêt

**Client :**

```erlang
-module(bob).

-export([borrow/2, start/0, loop/0]).

borrow(From, To) ->
    From ! {To, borrow}.

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
        crash ->
            atom = 2;
        Book -> 
            io:format("~p received ~n", [Book]),
            loop()
    end.
```

Le client peut afficher les livres qu'il emprunte et crasher.

Pour l'instant le lien `link` est en commentaire :

```erlang
3> Bob = bob:start().
<0.90.0>
4> App = app:start().
<0.92.0>

%% maintenant faisons crasher bob
6> Bob ! crash.
crash
=ERROR REPORT==== 14-Jun-2019::16:11:58.993712 ===
Error in process <0.90.0> with exit value:
{{badmatch,2},[{bob,loop,0,[{file,"bob.erl"},{line,14}]}]}
```

Voyons si `app` est toujours là :

```erlang
7> i().
<0.92.0>              app:init/0                             233        7    0
                      app:loop/1                               3              
```

### Maintenant dé-commentons `link`

```erlang
loop(Books) ->
    receive
        {Pid, borrow} ->
            {NewBooks, Book} = take_book(Books, Pid),
            link(Pid),
            ...
```

```erlang
11> App = app:start().
<0.108.0>
12> Bob = bob:start().
<0.110.0>
13> bob:borrow(App, Bob).
"alice in wonderlang" received 
{<0.90.0>,borrow}
```

Il faut emprunter un livre pour que le lien se crée

```erlang
14> Bob ! crash.
=ERROR REPORT==== 14-Jun-2019::16:20:01.393359 ===
Error in process <0.110.0> with exit value:
{{badmatch,2},[{bob,loop,0,[{file,"bob.erl"},{line,14}]}]}

crash
15> i().
...
<0.78.0>              erlang:apply/2                        1598    82058    0
                      c:pinfo/1                               50 
```

le dernier Pid est `<0.78.0>`, le crash de `Bob` a donc entrainé `App`.

## `process_flag(trap_exit, true)` transforme le crash en message

`process_flag`  transform la propagation du crash en message `{'EXIT', Pid, Reason}`

Modification du code :

```erlang
...
init() ->
  	%% On transforme le crash en message
    process_flag(trap_exit, true),
    Books = {[], ["alice in wonderland", "bilbo the hobbit", "black dhalia"]},
    loop(Books).

loop(Books) ->
    receive
        {Pid, borrow} ->
            {NewBooks, Book} = take_book(Books, Pid),
            link(Pid),
            Pid ! Book,
            loop(NewBooks);
      %% on reçoit le message du process qui a crashé
        {'EXIT', Pid, Reason} ->
            io:format("Bob chashed pid : ~p reason : ~p ~n", [Pid, Reason]);
        stop ->
            ok
    end.
 ...
```

```erlang
8> bob:borrow(App, Bob).
"alice in wonderlang" received 
{<0.100.0>,borrow}
9> Bob ! crash.
Bob crashed pid : <0.100.0> reason : {{badmatch,2},
                                      [{bob,loop,0,
                                        [{file,"bob.erl"},{line,14}]}]} 
=ERROR REPORT==== 14-Jun-2019::16:42:10.891432 ===
Error in process <0.100.0> with exit value:
{{badmatch,2},[{bob,loop,0,[{file,"bob.erl"},{line,14}]}]}

crash
```

Les deux processus ont crachés :

```erlang
i().

%% ...
<0.90.0>              app:init/0                             233       16    0
                      app:loop/1                               3              
<0.92.0>              bob:loop/0                             233       32    0
                      bob:loop/0                               1              
Total                                                      43822   501481    0
```

```erlang
7> Bob ! crash.
Bob crashed pid : <0.92.0> reason : {{badmatch,2},
                                     [{bob,loop,0,
                                       [{file,"bob.erl"},{line,14}]}]} 
=ERROR REPORT==== 17-Jun-2019::10:20:06.079211 ===
Error in process <0.92.0> with exit value:
{{badmatch,2},[{bob,loop,0,[{file,"bob.erl"},{line,14}]}]}

crash
```

```erlang
i().

<0.78.0>              erlang:apply/2                        2586    81202    0
                      c:pinfo/1                               50              
Total                                                      43213   591018    0
```

