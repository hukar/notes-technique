# 02 fragments

Si on a plusieurs éléments dans un composant, on est obligé de mettre un élément parent autour :

```jsx
function Name({ style, children, age }) {
    return (
        <div>
            <h3 style={style}>{children}</h3>
            <h4>{age}</h4>
        </div>
    );
}
```

On peut utiliser `Fragments` pour éviter d'avoir plein de `div` dans son rendu :

```jsx
import React, { Fragment } from "react";

function Name({ style, children, age }) {
    return (
        <Fragment>
            <h3 style={style}>{children}</h3>
            <h4>{age}</h4>
        </Fragment>
    );
}
```

Autres syntaxes :

```jsx
// plus besoin d'importer {Fragment}
<>
	<h3 style={style}>{children}</h3>
	<h4>{age}</h4>
</>

// ou encore la syntaxe tableau
return [<h3 style={style}>{children}</h3>, <h4>{age}</h4>];
```

### Traduction `Babel`

```jsx
<>
	<h1>hello</h1> 
</>
```

```js
React.createElement(
  React.Fragment, 
  null, 
  React.createElement(
    "h1", 
    null, 
    "hello")
);
```

On voit le fameux `React.Fragment`

