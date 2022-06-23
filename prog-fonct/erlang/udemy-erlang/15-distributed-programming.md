# La programmation distribuée

```erlang
-module(talk).

-export([run/0,alice/0,bob/2,startAlice/0,startBob/1]).

alice() ->
    receive
        {message, BobNode} ->
            io:fwrite("Alice got a message\n"),
            BobNode ! message,
            alice();
        finished -> io:fwrite("alice is finished\n")
    end.

bob(0, AliceNode) ->
    {alice, AliceNode} ! finished,
    io:fwrite("bob is finiched\n");
bob(N, AliceNode) ->
    {alice, AliceNode} ! {message, self()}, 
    receive
        message -> io:fwrite("Bob got a message\n")
    end,
    bob(N-1, AliceNode).

run() ->
    register(alice, spawn(talk, alice, [])),
    register(bob, spawn(talk, bob, [3])).

startAlice() ->
    register(alice, spawn(talk, alice,[])).

startBob(AliceNode) -> %% node == Host Address
    spawn(talk, bob, [3, AliceNode]).
```

## Changer de nom d'hôte

```bash
sudo scutil --set HostName titi
```

## Lancer Erlang avec un nom particulier

```erlang
erl -sname bxl
```

`-name` si c'est deux machine distinct `-sname` si on simule sur la même machine.

```bash
Eshell V10.3.2  (abort with ^G)
(bxl@titi)1>
```

J'obtiens `bxl@titi`

J'ouvre deux shell

```bash
erl -sname uccle
(uccle@titi)1> c(talk).
{ok,talk}
(uccle@titi)3> talk:startAlice().
true
```

L'autre :

```bash
erl -sname bxl
(bxl@titi)1> c(talk).
{ok,talk}
(bxl@titi)2> talk:startBob(uccle@titi).
<0.91.0>
```

AliceNode set donc remplacé par `uccle@titi`

Et ensuite le programme se déroule sur chaque terminal :

```bash
Alice got a message
Alice got a message
Alice got a message
alice is finished
```

```bash
Bob got a message
Bob got a message
Bob got a message
bob is finiched
```

