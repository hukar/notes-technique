# 01 Erlang Bollean

Toujours utiliser les opérateurs strictes sauf volonté spécifique :

```erlang
15> 5=:=5.0.
false
16> 4=/=4.0.
true
```

### ! attention à `>=` et `=<` 

## Comportement de true et false avec des autres types

```erlang
9> 0==false.
false
10> 1<false.
true
11> 5<false.
true
```

`true` et `false` sont en fait des `atoms`.

### hiérarchie entre les types :

````erlang
number < atom < reference < fun < port < pid < tuple < list < bit string
````

````erlang
> T = {yo, coco}.
{yo,coco}
> yo < T.
true
> 12 > T.
false
> T < L.
true
> L > coco.
true
````

