# Un nombre aléatoire en javascript

```js
// un nombre entre a et b

Math.floor(Math.random() * (b - a)) + a;
```

### Pourquoi `Math.floor` et pas `Math.ceil`

`Math.random` renvoie un `float` entre `[0, 1[`

`0` compris

`Math.ceil` de `Math.random()*10` va donc arrondir au supérieur, comme il y a une probabilité (très très faible) d'obtenir `0`, on a donc comme interval d'arrivé `[0, 10]`, mais avec une chance très très faible d'avoir zéro, le problème de répartition est résolu en utilisant `Math.floor`

