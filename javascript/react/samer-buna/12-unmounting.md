# 12 `Un-mounting` et `remounting`

Plutôt que de faire une fonction de type `resetGame` :

```js
const resetGame = () => {
    setStars(utils.random(1, 9));
    setAvailableNums(utils.range(1, 9));
    setCandidateNums([]);
    setSecondLeft(10);
};
```

où on remet dans les conditions initiales le composant, on peut juste le `dé-monter` et le `re-monter`.

## Le `key` attribute

On peut se servir de l'attribut `key` pour indiquer à **React** que ce n'est plus le même composant.

Si le `key` a une valeur différente, **React** va `un-mount` l'ancien composant et `remount` le nouveau.

En faisant cela, les valeurs de départ sont initialisées sans utiliser une fonction de `reset`.

```jsx
const StarMatch = () => {
    const [gameId, setGameId] = useState(1);

    return <Game key={gameId} startNewGame={() => setGameId(gameId + 1)}/>;
}
```

On voit que la seule responsabilité du composant parent est de monter et démonter le composant enfant grace à l'attribut `key` et au passage d'une fonction modifiant la valeur de l'attribut `key`.

