# 08 React `State` Patterns

## Ne pas mettre à jour son `state` avec une ancienne valeur du `state`

exmple un compteur :

```jsx
// App.js

class App extends Component {
    constructor(props) {
        super(props);
        this.state = { counter: 0 };
        this.increment = this.increment.bind(this);
    }

    increment() {
        this.setState({ counter: this.state.counter + 1 });
    }
  
    render() {
        return (
            <div className="App">
                <p>Counter : {this.state.counter}</p>
                <p>
                    <button onClick={this.increment}>Add One</button>
                </p>
            </div>
        );
    }
}
```

Se code fonctionne correctement, mais on change la valeur du state avec sa valeur précédente :

```jsx
this.setState({ counter: this.state.counter + 1 });
```

### Ajoutons un deuxième bouton :

```jsx
// ...
addThree() {
  this.setState({ counter: this.state.counter + 1 });
  this.setState({ counter: this.state.counter + 1 });
  this.setState({ counter: this.state.counter + 1 });
}

// ...
<button onClick={this.addThree}>Add Three</button>
```

Ici le résultat n'est pas celui attendu, si on appuie sur `Add One` le compteur passe à un, mais si on appuie sur `Add Three` le compteur passe seulement à deux !

#### La raison : `setState` est asynchrone

Il set donc risqué de supposé que le dernier appel est terminé, lorsqu'on lance le deuxième.

Pour des raisons de performances, **React** ne va exécuter que le dernier appel (d'où un seul `render`).

Si l'appel de `setState` dépend de la valeur courante du `state` , alors il est plus sûre d'uttiliser la forme `callback` de `setState`

```
this.setState(callback)
```

À la place de passer un objet, on passe une fonction `callback`

```jsx
this.setState(curState => ({ count: curState.count + 1}));
```

`({ count: curState + 1})` ici les parenthèses servent à différencier les accolades de l'objet avec celle du scope de la fonction anonyme.

`( ... )` est une expression en javascript.

```jsx
addThree() {
  this.setState(st => ({ counter: st.counter + 1 }));
  this.setState(st => ({ counter: st.counter + 1 }));
  this.setState(st => ({ counter: st.counter + 1 }));
}
```

Le code ci-dessus donne le résultat attendu.

## Abstraire la mise à jour du `state`

### Pattern : functional `setState`

On peut représenter la mise à jour du `state ` par une fonction séparée :

```jsx
// elsewhere in the code
function incrementCounter(prevstate) {
  return {count: prevState.count + 1};
}

// somewhere in the component
this.setState(incrementCounter);
```

Ce **pattern** est utile pour les tests :

```js
expect(incrementCounter({count: 0})).toEqual({ count : 1});
```

Il apparaît aussi tout le temps dans **Redux**

```jsx
import React, { Component } from "react";
import "./App.css";

function increment(prevState) {
    return { counter: prevState.counter + 1 };
}

class App extends Component {
    constructor(props) { // ...
```

On place la fonction en dehors du composant, puis dans celui-ci :

```jsx
addOne() {
  this.setState(increment);
}

addTwo() {
  this.setState(increment);
  this.setState(increment);
}
```

## Structures de données mutables

### Mauvaise manière de modifier

```jsx
handleTodo = (id) => {
        
  let t = this.state.todo.find(t => t.id === id);
  t.done = !t.done;
  this.setState({ todo: this.state.todo });
};
```

On ne modifie pas directement par référence en **React**, il est préférable de passer par une copie de la structure de données.

### Mise à jour du `state` immutable

```jsx
handleTodo = id => {
  const newTodo = this.state.todo.map(t => {
    if (id === t.id) {
      return { ...t, done: !t.done };
    }
    return t;
  });
  this.setState({ todo: newTodo });
};
```

`{ ...t, done: !t.done}` : avec le spread operator (propager) on copie les attributs de t dans un nouvel objet litérale, puis on écrase la valeur de `done` avec une nouvelle valeur.

#### Exemple avec un tableau

```jsx
addIcons() {
        const idx = Math.floor(Math.random() * this.props.options.length);
        const newIcons = this.props.options[idx];
        this.setState({ icons: [...this.state.icons, newIcons] });
    }
```

`[...this.state.icons, newIcons]` on fait la copie des éléments du tableau icons dans un nouveau tableau litérale, et on ajoute à la fin `newIcons`.

## Opérateur de copie immutable

`.map`  `.filter` `.reduce` = pure function

`...spread operator`

Il y a un coût en compléxité et en temps O(n), mais cela avut toujours mieux que des bugs compliqués dû à des effets de bord.

## Le `state` est immutable

L'ancien objet state est remplacé par un nouveau, les deux étants des instantannés dans le temps.

### `setState({oldObject: newObject})`



## Designing `state`

Il faut garder le `state` le plus petit possible

Si `x` est modifié, alors il est dans le state 

si `x` n'est pas modifié ou peut être obtenu par calcul, c'est une `prop`

### Mauvais exemple

```jsx
this.state = {
  firstName: "john",
  lastName: "Doe",
  birthday: "1955-10-11",
  age: 64,
  mood: "insane"
}
```

`firstName`, `lastName` et `birthday` ne change pas, il ne doivent pas être dans le `state`.

`age` peut être obtenu par calcul avec `birthday`, il ne doit pas être non plus dans le `state`

### Correction

```jsx
console.log(this.props);
{
  firstName: "john",
  lastName: "Doe",
  birthday: "1955-10-11",
  age: 64
}

console.log(this.state);
{
  mood: "insane"
}
```

#### Le `state` doit rester le plus petit possible

## Le `state` doit vivre chez le parent

Il vaut mieux centraliser le `state` chez un composant parent pour respecter le pattern ***"downward data flow"*** = flux de données vers le bas.

Un composant parent **statefull** vers ses enfant **stateless**

#### le `state` doit être centralisé dans le parent

