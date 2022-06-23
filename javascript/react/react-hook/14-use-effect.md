# 14 Le hook de cycle de vie : `useEffect`

Avec les `hooks`, on peut en avoir plusieurs en même temps.

## `useEffect`

#### Est appelée à chaque nouvel affichage (après chaque mise à jour du DOM).

On passe un tableau de variable à surveiller.

Le `hook` `useEffect` se déclenche à chaque modification de ses variables.

Si le tableau est vide, `useEffect` ne se joue qu'une seule fois lorsque le composant est montré dans le `DOM`.

Si on ne fournit pas de tableau, `useEffect` se déclenche à chaque modification de l'état.

la fonction de `useEffect` renvoie une fonction de `cleanup`.

## Naissance d'un composant `on mount`

`useEffect` prend une fonction de retour et un tableau en argument.

Si le tableau est vide `useEffect` ne lancera sa fonction qu'une seule et unique fois à la naissance du composant :

```jsx
import React, { useEffect } from 'react';

function App() {

  useEffect(() => {
    console.log("born");
  }, []);
```

## mise à jour `update`

Si on ne passe pas de deuxième argument à `useEffect`, la fonction de `callback` sera appelée au tout début :  `on mount` puis à chaque mise à jour du `state` : `update`

```jsx
useEffect(() => {
    console.log("growing ", growth);
});
```

Pour éviter les boucles d'appelle infini, dans certain cas `React` envoie un `Warning` si le deuxième argument n'est pas renseigné.

## Déclarer un événement particulier

Si on fournit une variable au tableau, si cette variable est modifiée, la fonction `callback` est appelée.

```jsx
const [nirvana, setNirvana] = useState(false);

useEffect(() => {
    if(born) document.title = "nirvana attained";
}, [nirvana]);
```

Plus loin pour déclencher l'effet :

```jsx
useEffect(() => {
    born ? console.log("growing ", growth): born = true;

    if(growth > 7) setNirvana(true);
  }, [growth]);
```

## Fonction de `cleanup`

```jsx
useEffect(() => {
  console.log("flag is called");

  console.log("before return callback flag");

  return () => {
    console.log("clean up clean up");
  };
}, [flag]);
```

```bash
clean up clean up
flag is called
before return callback flag
```

On voit que le callback est appelé en premier.

Je ne comprends pas l'intérêt.