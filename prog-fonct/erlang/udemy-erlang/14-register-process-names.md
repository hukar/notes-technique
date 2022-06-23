# 14 Registre des noms de processus

`register` est une fonction de Erlang qui permet d'associer un processus à un `atom`et de l'identifier plus facilement qu'avec son Pid.

```erlang
-module(talk).

-export([run/0,alice/0,bob/1]).

alice() ->
    receive
        message ->
            io:fwrite("Alice got a message\n"),
            bob ! message,
            alice();
        finished -> io:fwrite("alice is finished\n")
    end.

bob(0) ->
    alice ! finished,
    io:fwrite("bob is finiched\n");
bob(N) ->
    alice ! message, 
    receive
        message -> io:fwrite("Bob got a message\n")
    end,
    bob(N-1).

run() ->
    register(alice, spawn(talk, alice, [])),
    register(bob, spawn(talk, bob, [3])).
```

Le code est plus lisible.

##`register(atom_name, spawn(module, function , parameters))`

