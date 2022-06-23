# le `setter` de `useState` est asynchrone

En pratique, si le nouveau state dépend de l'ancien, il faut passer une fonctione au `setter`.

## Exemple

On va ajouter un élément à une liste :

```jsx
import React, { useState } from "react";

function List() {
    const [list, setList] = useState([0, 1, 2, 0]);

    const increment = () => {
        setList([...list, 1]);
    };

    return (
        <>
            <p>{list}</p>
            <p>
                <button onClick={increment}>Increment</button>
            </p>
        </>
    );
}
```

On utilise directement la valeur de `list` pour mettre à jour notre `state`

#### `setList([...list, newItem])`

Sortie :

```bash
[0, 1, 2, 0, 1]
```

### Maintenant modifions `increment`

```jsx
const increment = () => {
  setList([...list, 1]);
  setList([...list, 1]);
  setList([...list, 1]);
};
```

sortie :

```bash
[0, 1, 2, 0, 1]
```

#### ?? bug, le `state` étant asynchrone, `list` garde toujours son ancienne valeur et seul le dernier appel est conservé.

```jsx
const increment = () => {
  setList([...list, 1]);
  setList([...list, 1]);
  setList([...list, 5]);
};
```

sortie :

```bash
[0, 1, 2, 0, 5]
```

Comme prévu seul le dernier appel est conservé.

## Correction du problème : passer une fonction

```jsx
const increment = () => {
        setList(prevList => [...prevList, 1]);
        setList(prevList => [...prevList, 1]);
        setList(prevList => [...prevList, 5]);
    };
```

On passe ici une fonction en argument qui part du `state` passé pour retourner le nouveau `state`.

Sortie :

```bash
[0, 1, 2, 0, 1, 1, 5]
```

Ce qu'on attendait comme résultat.