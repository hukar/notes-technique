# Traitement des erreur de Processus

## link et exit signals

Propagation transitive

![Capture d’écran 2019-05-21 à 11.20.42](assets/schema_link.png)

### link/1 et spawn_link/3



### process_flag(trap_exit, true)



## Terminologie

### Link 

chemin de propagation bi-directionnel pour les messages de sortie transmis entre les processus

### Exit Signal

C'est un signal transmit par un processus sortant. Il contient les informations de fin.

### Error Trapping

L'habiliter d'un processus à traiter les signaux de sortie comme des messages normaux

## Fonctions utiles BIF

### link(Pid)

Créer un lien entre le processus appelant et le Pid.

Apparemment ne fonctionne pas avec un alias créé avec register(alias_atom, Pid).

### unlink(Pid)

Supprime le lien.

#### Spawn_link(Mod, Fun, Args)

Crée un lien avec le processus appelant après avoir générer le processus.

### process_flag(trap_exit, true | false)

Oblige le processus courant  à convertir les signaux de sortie en messages de sortie.

### exit(Reason)

finit le processus avec la raison `Reason`

### exit(Pid, Reason)

Force un processus d'id Pid à sortir avec la raison `Reason`

```
## Transformation d'un signal de sortie en message de sortie. ##
################################################################

exit(Pid, Reason)

process_flag(trap_exit, true)

{'EXIT', Pid, Reason}
```

## Sémantique de la propagation

| raison de la sortie | Trap_exit : trapping exists (piègeage de la sortie) | Not trapping exists (pas de piègeage de la sortie) |
| ------------------- | --------------------------------------------------- | -------------------------------------------------- |
| Normal              | I.<br />receives<br />{'EXIT', Pid, normal}         | II.<br />rien ne se passe                          |
| Kill                | III.<br />finit avec la raison kill                 | IV.<br />Finit avec la raison killed               |
| Autre               | V.<br />receives<br />{'EXIT', Pid, Other}          | VI.<br />finit avec la raison autre.               |

I.

```erlang
-module(exit).

-export([start/0, one/0, two/0]).

start() ->
    register(one, spawn(?MODULE, one, [])).

one() ->
    spawn_link(?MODULE, two, []),

    receive
        stop ->
            io:format("one stopped ~n");
        _ ->
            io:format("hello I'm one ~n"),
            one()
    end.

two() ->
    process_flag(trap_exit, true),

    receive  
        {'EXIT', Pid, Reason} ->
            io:format("two: ~w exit, ~w ~n", [Pid, Reason])
    end.
```

```
1> c(exit).
{ok,exit}

2> exit:start().
true

3> one ! hello.
hello I'm one 
hello

4> one ! stop.
one stopped 
stop
two: <0.85.0> exit, normal 
two: <0.85.0> exit, normal 
```

Réponse attendue sauf qu'elle est double ??

II.

```erlang
two() ->
    % process_flag(trap_exit, true), % <- ici on commente le process_flag

    receive  
        {'EXIT', Pid, Reason} ->
            io:format("two: ~w exit, ~w ~n", [Pid, Reason])
    end.
```

```
7> one ! hello.
hello I'm one 
hello

8> one ! stop.
one stopped 
stop
```

Rien ne se passe.

III.

```erlang
one() ->
    spawn_link(?MODULE, two, []),

    receive
        stop ->
            io:format("one stopped ~n"),
            exit(kill); % ici exit(kill)
        _ ->
            io:format("hello I'm one ~n"),
            one()
    end.

two() ->
    process_flag(trap_exit, true), % trap_exit

    receive  
        {'EXIT', Pid, Reason} ->
            io:format("two: ~w exit, ~w ~n", [Pid, Reason])
    end.
```

```
11> one ! hello.
hello I'm one 
hello

12> one ! stop.
stop
one stopped 
two: <0.107.0> exit, kill 
two: <0.107.0> exit, kill 
```

Finit avec la raison kill (toujours cette répétition ??)

IV.

```erlang
two() ->
    % process_flag(trap_exit, true),
 ...
```

```
15> one ! hello. 
hello I'm one 
hello

16> one ! stop.  
one stopped 
stop
```

`two: <0.107.0> exit, killed` était le résultat attendu ??

V.

```erlang
one() ->
    spawn_link(?MODULE, two, []),

    receive
        stop ->
            io:format("one stopped ~n"),
            exit(error_bad_food);
...

two() ->
    process_flag(trap_exit, true),
...
```

```
20> one ! stop.  
one stopped 
stop
two: <0.129.0> exit, error_bad_food 
two: <0.129.0> exit, error_bad_food 
```

Attendu sauf le doublet (?).

VI.

```erlang
24> one ! stop.  
one stopped 
stop
```

Comment voire la raison de fin ? (Normalement other)