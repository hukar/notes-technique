# Erlang et la concurrence II

## Registered Processes Processus inscrit (dans le registre des processus)

### Trois fonctions BIF (Built In Function) ...

#### `register` enregistre un nom de processus

```
register(Alias, Pid)

Alias ! message
```

```erlang
register(alice, spawn(recap, alice, [])).
true

alice ! {self(), hello}.
alice dit hello 
{<0.78.0>,hello}
```

#### `registered`  retourne tous les noms de processus enregitrer

```erlang
register(alice, spawn(recap, alice, [])).
true

register(polo, spawn(recap, polo, [])).
true

registered().
[application_controller,erts_code_purger,init,
 standard_error,erl_signal_server,user,inet_db,
 kernel_safe_sup,standard_error_sup,logger_std_h_default,
 logger_sup,logger_proxy,logger_handler_watcher,logger,
 erl_prim_loader,polo,rex,global_group,alice,kernel_sup,
 kernel_refc,user_drv,global_name_server,file_server_2,
 code_server]
```

On retrouve polo et alice.

#### `whereis`  retourne le Pid

```erlang
register(lucas, spawn(?MODULE, lucas_fun, []))
Pid = whereis(lucas)
```



```erlang
whereis(alice).
<0.85.0>

whereis(polo).
<0.87.0>
```

### Communication entre Al et Caty

```erlang
start() ->
    register(al, spawn(?MODULE, al, [])),
    register(caty, spawn(?MODULE, caty, [])). %% register pour enregistrer le label

al() ->
    receive
        caty ->
            Pid = whereis(caty), %% whereis pour retrouver le Pid du label
            Pid ! hello_caty
    end,
    al().

caty() ->
    receive
        hello_caty ->
            io:format("caty receives hello caty ~n")
    end,
    caty().
```

```erlang
3> demo1:start().
true

4> al ! caty.
caty receives hello caty 
caty
```



## Exemple avec `register`

```erlang
-module(register).

-export([start/0, analyse/1, server/0]).

start() ->
        Pid = spawn(?MODULE, server, []),
        register(analyser, Pid). % on enregistre le processus avec le nom register

server() ->
    receive
        {Pid, {analyse, Seq}} ->
            io:format(" ~w analysé ~n", [Seq]), % deuxième paramètre = liste
            Pid ! {reply, sequence_analysed},
            server()
    end.

analyse(Seq) ->
    analyser ! {self(), {analyse, Seq}}, % on envoie au nom entregistré

    receive
        {reply, R} ->
            io:format("sequence : ~w ~n", [R])
    end.
```

```
c(register).
{ok,register}

register:start().
true

register:analyse([6, 7, 8]).
 [6,7,8] analysé 
sequence : sequence_analysed 
ok
```

#### ! Un processus enregistré auxquel on envoie un message et qui n'existe pas crée une erreur de type `badarg`

#### ! Au contraire d'un envoie à un processus qui n'existe pas et où aucune erreur n'est lancée

```erlang
Pid4 = pid(0, 78, 0). % je crée un numéro de processus
<0.78.0>

Pid4 ! hello. % le message est bien envoyé, pas d'erreur !
hello

tintin ! hello. % ici un nom enregistré n'existant pas va lancer une erreur
** exception error: bad argument
     in operator  !/2
        called as tintin ! hello
```

## Timeouts



```
receive
	Msg ->
		<expressions1>
after Timeout ->  % Timeout en ms
  <expressions2>
end.
```

exemple :

```erlang
-module(time).

-export([alice/0]).

alice() -> 
    receive
        hello -> 
            hello,
        alice()
    after 20000 -> % 20s
        io:format("too long time I'm waiting !! ~n")
    end.
```

#### ! quitter un processus : `exit(Pid, error)`

### Lister les processus `i()` :

```
i().

Pid                   Initial Call                          Heap     Reds Msgs
Registered            Current Function                     Stack              
<0.0.0>               otp_ring0:start/2                      376     1848    0
init                  init:loop/1                              2              
<0.1.0>               erts_code_purger:start/0              2586    44191    0
erts_code_purger      erts_code_purger:wait_for_request        0              
<0.2.0>               erts_literal_area_collector:start      233    43281    0
...
```

```
exit(<0.123.0>, msg_stop).
```

## Un timer

```erlang
-module(night).

-export([lupus/1]).

lupus(Pid) ->
    receive
    after 3000 -> 
        Pid ! aouuuuuh
    end.
```

```
38> c(night).
{ok,night}

39> night:lupus(self()).
	%% on attend 3s 
aouuuuuh
```

## Une fonction sleep

```
sleep(T) ->
	receive
	after T -> true
	end.
```

```erlang
sleep(T) ->
    receive
    after T ->
        true
    end.

speak() ->
    io:format("hello ... ~n"),
    sleep(1000),

    io:format("... I am a nice lycanthrope ... ~n"),
    sleep(2000),

    io:format(" ... and I'm hungry :)= ~n").
```

## flush

```erlang
flush() ->
    receive
        _-> flush()
    after
        0 -> ok
    end.
```

Vide la mailbox et envoie ok

## Résumé

### Processus

Il ne partage pas sa mémoire et a son propre ramasse-miette (garbage collector).

Extrémement légé en Erlang on peut en aoir des dixaines de milliers en même temps.

### Message

Le seul moyen de communiquer entre processus 

`!` Envoyer

 `receive` recevoir

### timeout

mechanisme d'attente du message entrant (incomming) dans un temps défini

```
receive
	M -> something();
after 1000 ->
	something_else()
end.
```

### Processus enregistré

`register(titi, Pid)`

`Pid = whereis(titi)`

Associer un Process Id avec un nom (un atom).

### Fin du processus

1. Un processus est dit finit normalement s'il n'y a plus de code a exécuter.
2. Il est finit anormalement s'il se produit une erreur à l'exécution (run time error occurs)

## schéma général

```erlang
start(Args) ->
  spawn(server, init, [Args]).
              
init(Args) ->
  state = initialize_state(Args)
  loop(State).
    
loop(State) ->
  receive
    {handle, Msg} ->
      NewState = handle(Msg, State),
      loop(NewState);
    stop ->
      terminate(State)
    end.

terminate(State) ->
  clean_up(State).
```

