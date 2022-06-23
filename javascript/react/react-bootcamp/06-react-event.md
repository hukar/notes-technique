# React Events

Les événements React sont écrits en **camelCase** :

```jsx
<button onClick={this.makeTimer.start}>Start</button>
<button onClick={this.makeTimer.stop}>Stop</button>
```

Ils prennent une fonction *callback* en argument.

## Exemple de timer aléatoire

Un timer affiche un nombre aléatoire entre 1 et `maxNum` toutes les 1000 ms (1 s)

```jsx
class Rando extends Component {
  makeTimer = {
    randoInterval: undefined,

    start: () => {
      console.log("start");
      this.randoInterval = setInterval(() => {
        const rand = Math.floor(Math.random() * this.props.maxNum + 1);
        this.setState({ num: rand });
      }, 1000);
    },
    stop: () => {
      console.log("stop");
      clearInterval(this.randoInterval);
    }
  };

  state = { num: 0 };

  render() {
    return (
      <div>
        <p>{this.state.num}</p>
        <p>
          <button onClick={this.makeTimer.start}>Start</button>
          <button onClick={this.makeTimer.stop}>Stop</button>
        </p>
      </div>
    );
  }
}
```

Utilisation des propriété de classe `prop = value;` qui est seulement possible avec `Babel` et `Create-React-App`.

Du coup on peut se passer du constructeur et déclaré son `state` directement:

```jsx
state = { num: 0 };
```

## Binder `this` dans une classe de composant

```jsx
import React, { Component } from "react";

export default class ButtonClass extends Component {
  constructor(props) {
    super(props);

    this.state = {
      clicked: false
    };
  }

  handleClicked(e) {
    this.state.clicked
      ? this.setState({ clicked: false })
      : this.setState({ clicked: true });
  }

  render() {
    return (
      <div>
        <h1>{this.state.clicked ? "Bastard" : "yop yop"}</h1>
        <button onClick={this.handleClicked}>
          click me bastard{"!".repeat(5)}
        </button>
      </div>
    );
  }
}
```

Lorsque `this.handleClicked` est exécuté, elle est dans le gestionnaire d'événement du bouton.

Le mot clé `this` n'est plus relié au contexte de classe.

Il faut le relier *'manuellement'*, par exemple (mais cela peut être directement dans le `<button>`) dans le constructeur :

```jsx
constructor(props) {
    // ...
    this.handleClicked = this.handleClicked.bind(this);
  }
```

D'après la documentation, on peut aussi faire ceci sans préjudice des performances :

```jsx
export default class ButtonClass extends Component {
  constructor(props) {
    super(props);

    this.state = {
      clicked: false
    };
  }

  handleClicked = e => {
    this.state.clicked
      ? this.setState({ clicked: false })
      : this.setState({ clicked: true });
  };

// ...
```

Cette syntaxe est expérimentale (utilisation de **Babel** obligé).

Cf autres syntaxe : https://fr.reactjs.org/docs/faq-functions.html

## La même chose en fonctionnel (avec les `hooks`)

```jsx
import React, { useState } from "react";

export default function ButtonClass() {
  const [clicked, setClicked] = useState(false);

  const handleClicked = e => {
    clicked ? setClicked(false) : setClicked(true);
  };

  return (
    <div>
      <h1>{clicked ? "Bastard" : "yop yop"}</h1>
      <button onClick={handleClicked}>click me bastard{"!".repeat(5)}</button>
    </div>
  );
}
```

Pas de `this`, pas de `bind ` :)

