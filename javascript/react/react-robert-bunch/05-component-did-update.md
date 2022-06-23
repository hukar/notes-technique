# 05 `componentDidUpdate`

Cette fonction est appelée après que le composant soit re-rendu suite à un changement (du `state` ou des `props`).

Elle n'est pas appelée lors du premier rendu.

## `componentDidUpdate(prevProps, prevState, snapshot)`

Trois arguments.

`snapshot` a une utilité particulière et rare (comme comparer les coordonnés du pointeur) et fonctionne avec la méthode de cycle de vie : `getSnapshotBeforeUpdate()`.

## Utilisation de `prevState`

Si on utilise `setState` dans la méthode `componentDidUpdate` on risque une boucle infinie.

Pour prévenir cela on utilise `prevState` pour voire si la valeur du `state` précédent est bien différente :

```js
componentDidUpdate(prevProps, prevState) {
    if (this.state.weather !== prevState.weather) {
        const isRaining = this.state.weather.includes("rain");

        if (isRaining) {
            this.setState({ isRainning: "Rain rain go away!" });
        }
    }
}
```

