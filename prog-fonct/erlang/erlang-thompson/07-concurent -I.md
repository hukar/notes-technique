# Erlang et concurrence I

## Crée des processus

Chaque processus à sa mémoire séparée et un ramasse miettes séparé.

La seule façon qu'a un processus pour communiquer avec un autre, c'est d'utiliser les messages.

`Pid` : Processus id, chaque processus est identifié par son Pid.

### `spawn` pour lancer un processus 

```
Pid2 = spawn(Module, Function, Arguments) % Arguments est une liste []
```

## Envoyer un message

### `Pid2 ! {self(), foo}`

`!` est l'opérateur d'envoie de messages.

Il peut envoyer n'importe quel type de valeur (ici un tuple).

`self()` retourne le Pid du code  où il est exécuté.

### ! `spawn` et `!`  n'échoue jamais

Pour `spawn` à l'exécution, le processus peut finir normalement ou anormalement.

Pour `!`, le message est envoyé même si le destinataire n'existe pas ou plus.

## Réception du message

```erlang
receive
	{reset, Board}			-> reset(Board); % clause separator
	{shut_down, Board}	-> {error, unknown_msg} % no separator in last clause
end
```

La syntaxe ressemble au `if` ou au `case`, elle utilise le **pattern matching**.

```
receive
	pattern1 ->
		<expression>,
		<expression>,
		...,
		<expression>;
	pattern2 ->
		<expression>,
		<expression>,
		...,
		<expression>;
	pattern3 ->
		<expression>,
		<expression>,
		...,
		<expression> % pas de séparateur pour la dernière clause
end.
```

Les pattern sont vérifié de manière séquentielle du premier au dernier.

Seule les clauses correspondantes seront exécutées.

Les messages *non-matché*  reste dans la mail-box (attention à leur quantité !)

```
receive
	Msg1 ->
		receive
			Msg2 -> ...
```

Pour exécuter dans un ordre particulier.

## Exemple de code

```erlang
-module(echo).

-export([go/0, loop/0]).

go() ->
    Pid = spawn(echo, loop, []), % on crée un processus
    Pid ! {self(), hello}, % on envoie son Pid : self()

    receive % on reçoit un message
        {Pid, Msg} ->
            io:format("~w~n", [Msg])
    end,

    Pid ! stop. % on envoie le message stop

loop() ->
    receive on reçoit des messages
        {From, Msg} ->
            From ! {self(), Msg},
            loop(); % on se relance de manière récursive
        stop ->
            true
    end.
```

## Autre exemple avec le processus du shell Erlang

```erlang
-module(conc0).

-export([main/1, start/0]).

start() ->
    spawn(?MODULE, main, [4]). % ?MODULE macro builtin de erlang renvoie un atom du nom du module ici conc0

main(T) ->
    receive
        {Pid, stop} ->
            io:format("T: ~w~n",[T]),
            Pid ! stopped;
        {Pid, N} when T > 15 ->
            self() ! {Pid,stop}, % on s'envoie un message
            Next = N + T,
            Pid ! before_stopped, % on envoie un message à l'autre processus
            main(Next); % relance récursive du processus
        {Pid, N} ->
            Next = N + T,
            Pid ! Next,
            main(Next) % relance récursive du processus
    end.
```

```erlang
c(conc0).
{ok,conc0}

Pid = conc0:start(). % on récupère le Pid du processus main
<0.100.0>

Self = self(). % on récupère le Pid du shell erlang
<0.87.0>

Pid ! {Self, 16}. % on envoie 16 au processus main
{<0.87.0>,16}
           
receive X -> X end. % on reçoit Next
20 % T = 4 N = 16 Next = 20

Pid ! {Self, 1}. %  T vaut 20 > 15  
T: 21 % on affiche la valeur de T
{<0.87.0>,1}
           
receive Y -> Y end.
before_stopped % on reçoit d'abord le message before_stopped 2e clause

receive Z -> Z end. % puis stopped 1ère clause
stopped
```

#### ! il faut bien relancé le processus après chaque traitement dans `receive`

#### ! Il peut y avoir plusieurs messages en attente dans sa mail-box, il faut plusieurs `receive` consécutifs pour tout lire

## Résumé

`self()` renvoie son propre Process ID.

`spawn(module, function,[arg1, arg2, …])` génère un processus.

`Pid ! <message>` envoie un message de n'importe quelle valeur d'erlang au processus d'ID Pid.

`receive`  va voire les messages reçu dans la mai-box.