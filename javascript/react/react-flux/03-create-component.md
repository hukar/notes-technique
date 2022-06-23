# 03 Créer des Composant

Une fonction composant commence toujours par une majuscule.

Comme ça elle ne peut pas être confondue avec un tag `HTML`.

`create-react-app` utilise `ES module`, ce qui signifie que tout ce qui est dans un fichier est **privé** par défaut.

On doit exporter ce que l'on veut rendre `public`.

```jsx
import React from "react";

const HomePage = (props) => (
    <div>
        <h1>Pluralsight Admonistration</h1>
        <p>React, Flux and React Router for ultra-responsive web apps.</p>
    </div>
);

export default HomePage;
```

Notre `entry point` pour `create-react-app` est `index.js`.

## `render`

```jsx
import { render } from "react-dom";
```

est équivalent à :

```jsx
import ReactDom from "react-dom";
const render = ReactDom.render;
```

