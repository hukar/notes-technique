# 07 `useState` hook

```js
const [state, setstate] = useState(initialState)
```

Pour utiliser les `hook` il faut les importer :

```jsx
import React, { useState } from "react";
```

Dans le composant (fonctionnel obligatoirement) :

```jsx
const [age, setAge] = useState(22);
```

On obtient un tableau avec la valeur du `state` et un `setter`.

On donne à `useState` l'état initial du `state`.

## Utiliser le `setter`

`setState` prends soit une nouvelle valeur, soit une fonction qui a comme argument l'ancien `state` et comme retour le nouveau

Exemple 1 :

```jsx
const determineAge = () => setAge(33);
```

Cela fonctionne car la nouvelle valeur ne dépend pas de l'ancienne.

Par contre :

```jsx
const incrementAge = () => setAge(age + 1);
```

Pourra produire des erreurs (bizarrerie) dans le genre de cas suivant :

```jsx
const incrementAge = () => { setAge(age + 1); setAge(age + 1); setAge(age + 1); };
```

Ici l'age ne sera incrémenté qu'une fois, car `setAge` est asynchrone.

Pour obtenir le résultat souhaité on doit utiliser la deuxième forme de `setAge` :

```jsx
const incrementAge = () => { setAge(oldAge => oldAge + 1); setAge(oldAge => oldAge + 1); setAge(oldAge => oldAge + 1); };
```

