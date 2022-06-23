# 02 Fonctions

## Fonction à clauses multiples

Même nombre de paramètre mais différents cas.

```erlang
-module(factorial).

-export([factorial/1]).

factorial(1) ->
    1;
factorial(N) ->
    N * factorial(N-1).
```

