# Functional Component

Props -> function -> jsx

## Syntaxe 1

`Component/Greet.js`

```jsx
import React from 'react'

export const Greet = () => <h2>hello greeter</h2>
```

Obligation d'importer avec le nom de la fonction : `Greet`

`App.js`

```jsx
import React from 'react';
import Greet from './Component/Greet'
import './App.css';

function App() {
  return (
    <div className="App">

      <Greet></Greet>
      
    </div>
  );
}

export default App;
```

## Syntaxe 2

`Component/Greet.js`

```jsx
import React from 'react'

function Greet() {
    return <h2>hello greeter</h2>
}

export default Greet
```

Avec `default export` on peut changer le nom de la fonction `Greet`

`App.js`

```jsx
import React from 'react';
import MyGreet from './Component/Greet'
import './App.css';

function App() {
  return (
    <div className="App">

      <MyGreet></MyGreet>
```

