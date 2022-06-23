# 03 Decomposition des `Props`

## Utilisation de l'opérateur spread `...`

Par facilité on peut récupérer tous les attributs en déstructurant `Props` :

Dans le parent :

```jsx
<section>
  <FormName
    type="text"
    placeholder="write your name"
    style={{ color: "red" }}
    />
</section>
```

Dans l'enfant `FormName.js`

```jsx
import React from "react";

export default function FormName(props) {
    return (
        <form>
            <input {...props} />
        </form>
    );
}
```

C'est une syntaxe `jsx` permettant de passer toutes les `props` facilement

## Comprendre comment le `spread operator` fonctionne avec le `jsx`

Voici l'objet de base :

```jsx
const o = {
    value: "toto",
    style: {
        color: "blue"
    },
    className: "tag-name"
};
```

On va le décomposer dans un `tag` :

```jsx
function NameTag({ firstName, lastName }) {
    return <div  {...o}>spread onside</div>
}
```

On récupère le `html` suivant dans le navigateur :

```html
<div value="toto" class="tag-name" style="color: blue;">spread onside</div>
```

#### ! C'est une syntaxe `jsx` , ce n'est pas le `spread operator` de `js`