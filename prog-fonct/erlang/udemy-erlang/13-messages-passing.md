# 13 Le passage de méssages entre processus

`Alice` et `Bob`

```erlang
-module(talk).

-export([run/0,alice/0,bob/2]).

alice() ->
    receive
        {message, Pid} ->
            io:fwrite("Alice got a message\n"),
            Pid ! message,
            alice();
        finished -> io:fwrite("alice is finished\n")
    end.

bob(0, Pid) ->
    Pid ! finished,
    io:fwrite("bob is finiched\n");
bob(N, Pid) ->
    Pid ! {message, self()}, %% self() send bob process ID
    receive
        message -> io:fwrite("Bob got a message\n")
    end,
    bob(N-1, Pid).

run() ->
    Pid = spawn(talk, alice, []),
    spawn(talk, bob, [3, Pid]).
```

`receive`  `end`  utilise le pattern matching pour traiter le message reçu.

`Pid !`  permet d'envoyer un message au Processus Id `Pid ! {advert, "ceci est un avertissement"}`  par exemple.

`spawn(module, function, lis of parameters)` 