# 00 Hooks

Les `hooks` apparaissent avec React 16.8

Les `hooks` authorise la logique de React (props, state, context, life cycle) dans les composants fonctionnels

Les `hooks` ne fonctionnent pas dans les classes.

## Pourquoi ?

1. Pas de classe, pas de problème avec `this`
2. Cela permet de paratager la logique du `state` : `stateful logic`  sans passer par un composant `wrapper`.
3. La logique peut être réunie dans une seule fonction.



## `useState`

### Déclaration d'un `hook`

```jsx
import React, {useState} from "react";

const [nomDuState, setNomDuState] = useState(initialState);
```

- `nomDuState` est le `state` en lui-même (les données).

- `setNomDuState` est une fonction chargée de modifier le `state`.

- `initialState` étant l'état des données (du `state`) de départ.

### Exemple

```jsx
import React, { useState } from "react";

export default function App() {
  const [cartItems, setCartItems] = useState([]);
  return (
    <div className="App">
      <p>Items in your cart: {cartItems.toString()}</p>
      <button onClick={() => setCartItems([...cartItems, "apple"])}>
        add apple
      </button>
    </div>
  );
}
```

`cartItems` est **immutable**, si on écrit :

```jsx
<button onClick={() => setCartItems(cartItems.push("apple"))}>
        add apple
      </button>
```

On obtient un message d'erreur :

```
TypeError: cartItems.push is not a function
```

On utilise donc le `spread operator` pour effectuer une copie du contenu de `cartItems` dans un nouveau tableau : `[...cartItems, "apple"]`

#### `[...cartItems, "apple"]`

## `useEffect`

Ce `hook` est exécuté à chaque fois qu'il y a un rendu.

Il permet d'enregistrer un `listener` de la bonne manière (?).

Il prend comme argument une fonction qui `return` un `callback`.

En pratique cela permet d'ajouter et de retirer le gestionnaire d'événement dans la même fonction.

### Exemple :

```jsx
import React, { useState, useEffect } from "react";

export default function App() {
  const [scrollDepth, setScrollDepth] = useState(0);

  function determineUserScrollDepth() {
    const scrolled = document.scrollingElement.scrollTop; // valeur du scroll
    setScrollDepth(scrolled);
  }

  useEffect(() => {
    window.addEventListener("scroll", determineUserScrollDepth);

    return () => {
      window.removeEventListener("scroll", determineUserScrollDepth);
    };
  });
  return (
    <div className="App">
      <header
        style={{
          position: "fixed",
          backgroundColor: "black",
          top: "0",
          padding: "12px"
        }}
        >
        You've scrolled this far : {scrollDepth}
      </header>
      <p>
        Un long texte ...
      </p>
    </div>
  );
}
```

