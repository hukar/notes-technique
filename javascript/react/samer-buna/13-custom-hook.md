# 13 custom hook

Un gros composant fait trois chose :

1. Il manage le `state` => `useState` , `setData`, effet de bord avec `useEffect`.
2. Il peut calculer diverses choses.
3. Il implémente la logique de l'interface.

Un `custom hook` est une fonction qui par convention commence par `use` et contient des `hooks`.

## Règle de `custom hook`

1. Commencer par `use`.
2. Ne pas utiliser les `hooks` dans des boucles ou des instructions `if`.





## Exemple de `custom hook`

L'intérêt des `custom hook` c'est qu'on peut encapsuler la logique et les `setters` pour n'exposer que les valeurs uttilisées par le composant.

Tout les `setSomething` ne sont plus disponible à l'extérieur du `custom hook`.

```js
const useGameState = () => {
    const [stars, setStars] = useState(utils.random(1, 9));
    const [availableNums, setAvailableNums] = useState(utils.range(1, 9));
    const [candidateNums, setCandidateNums] = useState([]);
    const [secondLeft, setSecondLeft] = useState(10);

    useEffect(() => {
        if (secondLeft > 0 && availableNums.length > 0) {
            const timerId = setTimeout(() => {
                setSecondLeft(secondLeft - 1);
            }, 1000);
            console.log("timerId", timerId);
            return () => clearTimeout(timerId);
        }
    });

    const setGameState = (newCandidateNums) => {
        if (utils.sum(newCandidateNums) !== stars) {
            setCandidateNums(newCandidateNums);
        } else {
            const newAvailableNums = availableNums.filter(
                (n) => !newCandidateNums.includes(n)
            );
            setStars(utils.randomSumIn(newAvailableNums, 9));
            setAvailableNums(newAvailableNums);
            setCandidateNums([]);
        }
    };

    return { stars, availableNums, candidateNums, secondLeft, setGameState };
};
```

On voit que le `custom hook` utilise le `state`, définie les `side effect`, gère le comportement de modification du `state` en renvoyant aussi une fonction `setter`.

Dans le composant c'est simple de l'utiliser :

```jsx
const Game = ({ startNewGame }) => {
    const {
        stars,
        availableNums,
        candidateNums,
        secondLeft,
        setGameState,
    } = useGameState();
    
    const candidatesAreWrong = utils.sum(candidateNums) > stars;
    // ...
    
    const onNumberClick = (number, currentStatus) => {
        // currentStatus => newStatus
        if (currentStatus === "used" || gameStatus !== "active") {
            return;
        }

        const newCandidateNums =
            currentStatus === "available"
                ? candidateNums.concat(number)
                : candidateNums.filter((cn) => cn !== number);

        setGameState(newCandidateNums);
    };
    // ...
```

