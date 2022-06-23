# 05 Le `State`

Les propriétés sont immutable (d'où l'utilité de `static defaultProps`) alors que le `State` varie tout le temps.

- Ce sont les données interne d'un composant
- Ces données changent dans le temps

### POJO

Acronyme signifiant plain old java object = un bon viel objet java

Ici Un Bon Veil Objet Javascript, ce qui signifie qu'il n'étends ou n'implémente aucune classe ou interface : un simple objet.

Le `State` est un POJO.

## Initialiser le `State`

On initialise e `State`  le plus tôt possible : dans le constructeur.

## Premier exemple : un compteur

```jsx
import React, { Component } from "react";
import "./App.css";

class App extends Component {
  constructor(props) {
    super(props);
    this.state = {
      numClicks: 0
    };
  }

  incrementClicks() {
    console.log("here!");
    this.setState({ numClicks: this.state.numClicks + 1 });
  }

  render() {
    return (
      <div className="App">
        <div>
          <button onClick={this.incrementClicks.bind(this)}>Click !</button>
        </div>
        <p>Number of click : {this.state.numClicks}</p>
      </div>
    );
  }
}

export default App;
```

Dans le constructeur on initialise un `state` :

```jsx
constructor(props) {
  super(props);
  this.state = {
    numClicks: 0
  };
}
```

On modifie le `state` dans un fonction avec `setState`

```jsx
incrementClicks() {
  console.log("here!");
  this.setState({ numClicks: this.state.numClicks + 1 });
}
```

`setState` lance un `render`  de la vue.

Dans la vue, on doit ***binder*** avec `this` car le this dans la fonction n'est plus défini dans la vue.

```jsx
<button onClick={this.incrementClicks.bind(this)}>Click !</button>
```

Si on ne ***bind*** pas on obtient cette erreur :

```
uncaught TypeError: Cannot read property 'setState' of undefined
```

Alors si on ne veut pas ***binder***, on peut utiliser une **arrow function** dont la valeur de `this` est celle du contexte extérieur (alors que dans une fonction anonyme classique `this` pointe vers l'objet global ou `undefined` en mode strict).

#### ! Si un composant est *stateless*, on n'est pas obligé de mettre un constructeur

Si on a un `state` par contre, on **doit** avoir un constructeur.

## Syntaxe alternative : expérimentale

```jsx
import React, { Component } from "react";

class Game extends Component {
  state = {
    score: 12,
    gameOver: false
  };
	// ...
}
```

On peut directement déclarer une propriété sans passer par le constructeur.

Mais **ATTENTION**, cette syntaxe n'est pas du javascript, elle est transpilée par Babel dans le cadre d'une application avec `Create-React-App`

## `super` et `props`

```js
class Component {
  constructor() {
    console.log("inside component constructor");
  }
}

class Game extends Component {
  constructor() {
    console.log("inside Game component");
  }
}

const chess = new Game();
```

```
ReferenceError: Must call super constructor in derived class before accessing 'this' or returning from derived constructor

ReferenceError : Doit appeler le constructeur super dans la classe dérivée avant d'accéder à 'this' ou de sortir du constructeur dérivé.
```

On doit donc écrire ceci dans la classe dérivée :

```js
constructor() {
  super();
  // ... ;
}
```

### `props`

Si on fait un console.log des props depuis le constructeur :

```jsx
class Demo extends Component {
  constructor(props) {
    super();

    console.log(this.props);
  }
  
  // ...
```

```
undefined
```

On obtient `undefined` !

En dehors du constructeur, les `props` sont pourtant bien disponibles.

```jsx
class Demo extends Component {
  constructor(props) {
    super();

    console.log(this.props);
  }

  render() {
    return (
      <div>
        <p>
          {this.props.animal} {this.props.food}
        </p>
      </div>
    );
  }
}
```

![Screenshot 2019-10-24 at 16.53.48](assets/Screenshot 2019-10-24 at 16.53.48.png)

### Si on veut accéder aux `props` dans le constructeur :

```jsx
constructor(props) {
  super(props);

  console.log(this.props);
}
```

```
{animal: "Bobcat", food: "Pineapple"}
```

Si on a besoin d'utiliser les `props` dans le constructeur `super(props)`

Sinon `super()`

## Modifier le `state`

```jsx
class Demo extends Component {
  constructor() {
    super();

    this.state = {
      name: "booba",
      age: 45
    };
  }

  render() {
    this.state.name = "Antoine"; // ne jamais faire ça !!!
    return (
      <div>
        {this.state.name} {this.state.age}
      </div>
    );
  }
}
```

### ! ce code va fonctionner mais il ne faut jamais modifier le `state` directement

D'ailleurs on reçoit un warning dans la console :

```
Do not mutate state directly. Use setState()
```

## `setState`

On utilise pas `setState` dans le constructeur :

```jsx
constructor() {
    super();

    this.state = {
      name: "booba",
      age: 45
    };
    this.setState({ name: "Tito" });
  }
```

Résultat :

![Screenshot 2019-10-24 at 17.08.07](assets/Screenshot 2019-10-24 at 17.08.07.png)

Le nom n'est pas changé et dans la console :

```
index.js:1375 Warning: Can't call setState on a component that is not yet mounted. This is a no-op, but it might indicate a bug in your application. Instead, assign to `this.state` directly or define a `state = {};` class property with the desired state`
```

 **Avertissement :** Impossible d'appeler `setState` sur un composant qui n'est pas encore monté. Ceci est une **non-op**, mais cela pourrait indiquer un bogue dans votre application. Au lieu de cela, affectez directement à `this.state` ou définissez une propriété de classe `state = {};` avec l'état souhaité.

`no-op` : une action qui ne fait rien.

On ne peut pas non plus mettre `setState` dans le render .

Comme `setState` appelle la méthode `render` du composant, cela va  créer une boucle infinie.

### `setState` prend un objet avec les changement voulu.

```jsx
this.setState({prop: newValue});
```



## State vs Props

| Terme | Structure | Mutable | Propos                                   |
| ----- | --------- | ------- | ---------------------------------------- |
| State | POJO {}   | Oui     | Gère les données changeante du composant |
| Props | POJO {}   | Non     | Gère la configuration du composant       |

### Principe du flux de données vers le bas : downward data flow

Un **pattern** de **React**, c'est de passer les données du `state` d'un parent aux `props` d'un enfant :

```jsx
class Parent extends Component {
  constructor(props) {
    super(props);
    this.state = {
      count: 5
    };
  }
  
  render() {
    return (
    <div>
      	<ChildComponent count={this.state.count} />
      </div>);
  }
}
```

