# 03 Github Cards App : Class Component

On va utiliser l'`API` public de **Github**.

## class component

```jsx
class App extends React.Component {
  
  render() {
    return (
      <div className="header">
        {this.props.title}
      </div>
    );
  } 
};

ReactDOM.render(
  <App title="The Github Cards App" />, 
  mountNode
);
```



Bien étendre la classe `React.Component`.

C'est la méthode `render` qui est chargée du rendu. C'est la seule méthode **obligatoire** pour React.

Les propriétés sont passées par `this.props`.

### `super` `props` et `constructor`

```jsx
class App extends React.Component {
  
  constructor() {
      this.state = {
        thoughts: [
          "first, I'm genius",
          "second, I'm genius"
        ]
      };
    }
  
  render() {
    
    return (
      <div className="header">
        <h1>{this.props.title}</h1>
        {this.state.thoughts.map(thought => <p>{thought}</p>)}
      </div>
    );
  } 
};

ReactDOM.render(
  <App title="The Github Cards App" />, 
  mountNode
);
```

Mon constructeur est tout nu, pas de `super()` et pas de `props`.

```
ReferenceError: Must call super constructor in derived class before accessing 'this' or returning from derived constructor
```

En javascript on ne peut utiliser `this` sans avoir apellé `super()` :

```jsx
  constructor() {
    super();
      this.state = { // ...
```

tout fonctionne.

Voyons maintenant en utilisant la valeur des `props` pour initialiser le `state` :

```jsx
constructor() {
    super();
    this.state = {
        customTitle: "Custom **" + this.props.title,
        thoughts: [
            "first, I'm genius",
            "second, I'm genius"
        ]
    };
}
```

```
TypeError: Cannot read property 'title' of undefined
```

Je passe donc `props` au `constructor`:

```jsx
constructor(props) {
    super();
    this.state = {
        customTitle: "Custom **" + props.title,
        thoughts: [
            "first, I'm genius",
            "second, I'm genius"
        ]
    };
}
```

Cela fonctionne bien maintenant. Toujours pas besoin de passer `props` à `super`.

Si `props` n'est pas passé à `super()`, `props` est bien défini mais pas `this.props`, ce qui peut être source de confusion :

```jsx
class Button extends React.Component {
  constructor(props) {
    super(); // 😬 We forgot to pass props
    console.log(props);      // ✅ {}
    console.log(this.props); // 😬 undefined 
  }
  // ...
}
```

```jsx
class Button extends React.Component {
  constructor(props) {
    super(props); // ✅ We passed props
    console.log(props);      // ✅ {}
    console.log(this.props); // ✅ {}
  }
  // ...
}
```

Voire l'article de **Dan Abramov** : 

https://overreacted.io/why-do-we-write-super-props/

Avec l'écriture des `class fields` sans constructeur, ce problème ne se pose plus.

```jsx
class App extends React.Component {
  
 state = {
      customTitle: "Custom **" + this.props.title,
      thoughts: [
        "first, I'm genius",
        "second, I'm genius"
      ]
    };
    
  
  render() {
    
    return (
      <div className="header">
        <h1>{this.props.title}</h1>
        <h2>{this.state.customTitle}</h2>
        {this.state.thoughts.map(thought => <p>{thought}</p>)}
      </div>
    );
  } 
};
```

