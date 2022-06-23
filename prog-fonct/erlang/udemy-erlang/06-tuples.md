# Les tuples

Une liste d'éléments disparate pouvant contenir tous les types.

```erlang
t = {{animal, "tigre"},{age, 56},7.8,false}
```

## Utilisation dans une fonction

```erlang
convert({farenheit, X}) ->
    Y = (X - 32) * 5 / 9,
    {celsius, Y};
convert({celsius, X}) ->
    Y = X *5 / 9 + 32,
    {farenheit, Y}.
```

```bash
temp:convert({celsius, 25}).
{farenheit,45.888888888888886}
```

