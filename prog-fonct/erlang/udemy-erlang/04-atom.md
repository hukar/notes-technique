# Le concept d'`atom`

En Erlang les `atoms` sont justes des noms.

Pour créer un `atom`, on utilise les minuscules

## Utilisation des `atoms` dans une fonction

```erlang
-module(temp).

-export([convert/2]).

convert(F, farenheit) ->
    (F - 32) * 5 / 9;
convert(C, celsius) ->
    C * 9 / 5 + 32.
```

```bash
1> c(temp).
{ok,temp}
2> temp:convert(25, celsius).
77.0
3> temp:convert(77, farenheit).
25.0
```

