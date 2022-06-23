# Le modèle client serveur

## Intro

IL y a deux modèles de communication client - serveur : synchrone et asynchrone.

Synchrone : on attend la réponse

Asynchrone : On attend pas la réponse

![Capture d’écran 2019-05-23 à 15.49.26](assets/Capture d’écran 2019-05-23 à 15.49.26.png)

## Squelette de processus (révision)

```erlang
start(Args) ->
  spawn(server, init, [Args]).
              
init(Args) ->
  State = initialyse_state(Args),
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

## Implémentation personnel de ce modèle serveur

```erlang
-module(server).

-export([start/1, init/1]).

start(Args) ->
    spawn(server, init, [Args]).

init(Args) ->
    State = initialyse_state(Args),
    loop(State).

loop(State) ->
    receive
        {biscuit,Qte} ->
            NextState = biscuit(Qte, State),
            loop(NextState);
        show -> 
            io:format("state : ~w ~n", [State]),
            loop(State);
        stop -> terminate(State)
    end.

terminate(State) ->
    clean_up(State).

initialyse_state(Args) ->
    [A, B, C] = Args,
    [{biscuit, A}, {pain_choco, B}, {croissant, C}].

% pour modifier la liste de tuple j'utilise une liste de compréhension avec un case
biscuit(Qte, State) ->
    [case {G, Q} of {biscuit,_} -> {biscuit, Qte}; _-> {G, Q} end  || {G, Q} <- State].

clean_up(State) ->
    io:format("résultat : ~w ~n", [State]).
```

```erlang
[case {G, Q} of {biscuit,_} -> {biscuit, Qte}; _-> {G, Q} end  || {G, Q} <- State]

[
case {Gateau, Quantite} of
	{biscuit,_} -> {biscuit, NouvelleQuantite}; % si matching renvoie avec la nouvelle Qté
	_ -> {Gateau, Quantite} % autres cas renvoie sans changement
end
|| {Gateau,Quantite} <- State % itère sur les couples {G,Q} de la liste State
]
```

![Capture d’écran 2019-05-21 à 17.36.00](assets/Capture d’écran 2019-05-21 à 17.36.00.png)

## Exercice Frequency

![Capture d’écran 2019-05-23 à 15.52.36](assets/Capture d’écran 2019-05-23 à 15.52.36.png)

### lists:keydelete/3

`lists:keydelete(Key, N, TupleList)`

Enlève la première occurence de clé `Key` à la liste de tuple `TupleList` et renvoie une nouvelle liste de tuple.

Mettre `N` à 1 (je ne sais pas pourquoi !?)

```erlang
1> L = [{chat, "minou"},{chien, "toto"},{chien, "grou"},{chat, "tigro"},{chat, "blue"}].
[{chat,"minou"}, {chien,"toto"}, {chien,"grou"}, {chat,"tigro"}, {chat,"blue"}]

2> LL = lists:keydelete(chat, 1, L).
[{chien,"toto"},{chien,"grou"},{chat,"tigro"},{chat,"blue"}]
```

### Le code

```erlang
-module(frequency).

-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
    register(frequency, spawn(?MODULE, init, [])).

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10, 11, 12, 13, 14, 15].

% client functions

stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

% we hide all message passing and the message protocol in a
% functional interface

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.

% functionnal wrapper for sending a message
reply(Pid, Message) ->
    Pid ! {reply, Message}.

%the main loop
loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

% internal functions
% helper function used to allocate and deallocate frequencies

allocate({[], Allocated}, Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid} | Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.
```

### `lists:member(Element, List)`

Regarde si un élément est dans une liste et retourne `true` ou `false`

```
16> L = [5,5,6,8,9,4].
[5,5,6,8,9,4]
17> lists:member(5, L).
true
18> lists:member(4, L).
true
19> lists:member(3, L).
false
```

Implémentation personnelle :

```erlang
% my implementation of lists:menber(Elem, List) for fun
memb(Elem, List) ->
    IntermediateList = [M || M <- List, M == Elem],
    length(IntermediateList) =/= 0.
```

