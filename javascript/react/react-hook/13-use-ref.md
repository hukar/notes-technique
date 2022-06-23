#  `useRef` : référence vers un élément

Ce `hook` nous permet de créer une référence vers un élément `HTML`

## 1 création du `hook useRef`

```jsx
import React, { useRef } from "react";

// ...

const nameRef = useRef();
```

## 2 Lien avec l'élément

```jsx
<input ref={nameRef} />
```

## Exemple d'utilisation pour naviguer dans les champs de formulaire avec la touche `enter`

```jsx
function Form() {
    const nameRef = useRef();
    const ageRef = useRef();
    const marriedRef = useRef();

    const handleKeyDown = e => {
        // if (e.keyCode === 13) {
        if (e.key === "Enter") {
            e.preventDefault();
            switch (e.target.id) {
                case "name":
                    ageRef.current.focus();
                    break;
                case "age":
                    marriedRef.current.focus();
                    break;
                case "married":
                    submitRef.current.focus();
                    break;
            }
        }
    };

    return (
        <>
            <input ref={nameRef} id="name" />
            <input ref={ageRef} id="age" />
            <input ref={marriedRef} type="checkbox" id="married" />
        </>
    );
}
```

`e.keyCode` code ASCCII

`e.key` le caractère au format `string`

`e.preventDefault();` prevent le rafraîchissement de la page avec `enter`

## Passer sa référence `forwardedRef`

On souhaite passer une `ref` d'un élément enfant à un élément parent :

### élément parent

```jsx
import React, { useRef } from "react";
import Child from "./components/Child";
import "./App.css";

function App() {
    const childRef = useRef(null);

    return (
        <div className="App">
            <Child
              id="child"
              placeholder="type first name here"
              style="form-control"
              ref={childRef}
              />
        </div>
    );
}

export default App;
```

On utilise le `hook` `useRef` : `const childRef = useRef(null)`

On place `ref={childRef}` comme propriété du composant enfant.

### Élément enfant

```jsx
import React from "react";

function Input(props, ref) {
    return <input ref={ref} {...props} />;
}

const ForwardedInput = React.forwardRef(Input);
export default ForwardedInput;
```

On récupère la `ref` : `Element(props, ref)`

`{ ... props }` on décompose les attributs

`ref={ref}` on lie la référence

`React.forwardRef`

