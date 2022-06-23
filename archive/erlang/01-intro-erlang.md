# 01 intro erlang

lancer le terminal :

```bash
erl
```



## Dans la console

```erlang
cd("/Users/hukar/Documents/programmation/erlang/").
```

se rendre à ce dossier

```erlang
c(monmodule).
```

compiler son module

```erlang
monmodule:mafonction().
```

exécute une fonction `mafonction` du module `monmodule`

```erlang
coco:module_info().
```

```erlang
[{module,coco},
 {exports,[{add,3},
           {add,2},
           {hello_world,0},
           {module_info,0},
           {module_info,1}]},
 {attributes,[{vsn,[317686958789080997999879408474043653172]}]},
 {compile,[{version,"7.2.6"},
           {options,[]},
           {source,"/Users/hukar/Documents/programmation/erlang/coco.erl"}]},
 {native,false},
 {md5,<<239,0,72,94,50,60,122,159,177,34,136,56,32,51,28,
        52>>}]
```

pour obtenir de l'info sur son module

## Mon module

fichier `coco.erl`

```erlang
-module(coco).

-export([hello_world/0]).

hello_world() -> 
    io:fwrite("hello coco\n").
```

fonction sur deux lignes

```erlang
add(A, B) ->
    hello_world(),
    A + B.
```

