# 08 `css`

Rappelle sur l'encapsulation des `css`.

Il est recommandé de créer un fichier `MonComposant.ccs` et de définir des classes à l'intérieur pour l'attribut `className` plutôt que de définir des styles avec l'attribut du même nom.

`BoilingVerdict.js`

```jsx
import React from "react";
import "./BoilingVerdict.css";

function BoilingVerdict({ celsius }) {
    const isBoiling = celsius >= 100;

    let className;

    if (isBoiling) {
        className="too-hot"
    }

    console.log("is boiling", isBoiling);
    return (
        <div className={className}>
            {isBoiling ? (
                <p>The water would boil</p>
            ) : (
                <p>The water would not boil</p>
            )}
        </div>
    );
}

export default BoilingVerdict;

```

#### `import "./BoilingVerdict.css";`



`BoilingVerdict.css`

```css
.too-hot {
    color: white;
    font-size: x-large;
    background-color: darkred;
}
```



L'idéal étant de mettre les deux fichiers ensemble, par exemple dans un dossier au nom du composant.