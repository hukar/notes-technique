# 04 jsx

## Sans jsx

```js
import React from 'react'

const Hello = () => {
    const h1 = React.createElement('h1', {id:'title',className:'font-blue'}, 'hello kiki')

    return React.createElement('div', null, h1)
}
```

Utilisation de `React.createElement('nom_de_l_element',{objet d'attributs=> cle:valeur}, 'noeud enfant)`

## Avec jsx

```jsx
import React from 'react'

const Hello = () => (
     <div>
         <h1 id="title" className="font-blue">Hello coco</h1>
     </div>
)
```

On utilise `className` (en `camelCase` <> `PascalCase`) car `class` est un mot réservé en `javascript`.

## Différence avec HTML

`class` -> `className`

`for` -> `htmlFor`

convention de nommage des propriétés en `camelCase`

- `onclick` -> `onClick`

- `tabindex` -> `tabIndex`

