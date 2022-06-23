# `useState` Hook

## Version avec une classe

```jsx
class Counter extends React.Component {
  constructor(props) {
    super(props);
    // 1. On déclare un state
    this.state = {
      // 2. on l'initialise
      count: 0
    };
  }

  // 3. on construit une méthode
  addOne = () => {
    this.setState({ count: ++this.state.count });
  };

  render() {
    return (
      <div>
        <p>{this.state.count}</p>
        <br />
        <button onClick={this.addOne}>Add One</button>
      </div>
    );
  }
}
```

## Version avec les `hooks`

```jsx
function HookCounter() {
  //array destructuring
  // 1 et 2. déclaration et initialisation de count et de la méthode setCount
  const [count, setCount] = React.useState(0);

  return (
    <div>
      <!-- 3. implémentation de la méthode -->
      <button onClick={() => setCount(count + 1)}>Hook Counter {count}</button>
    </div>
  );
}
```

#### ! rappel js : affectation par décomposition

```js
const t = ["sara", "rosa"];

const [firstGirl, secondGirl] = t;

firstGirl; // sara
secondGirl; // rosa
```

#### `React.useState(value)`  renvoie un tableau avec une référence vers value et une référence vers le `setter` de value.

## Règles à suivre 

1. **Appeler les `hooks` seulement en haut de la fonction composant**
   Pas dans une boucle, dans une condition ou dans une fonction imbriquée.
2. **Appeler les `hooks`  seulement dans les fonctions composant React**
   Ne pas les appeler dans des fonctions normal javascript.

## Utilisation du `setter`

Créons un bouton pour ajouter 5 à un compteur, au lieu de fair `+ 5` , nous répétons `+ 1` 5 fois

```jsx
const HookCounterTwo = () => {
  const [count, setCount] = React.useState(0);

  const incrementFive = () => {
    for (let i = 0; i < 5; i++) {
      setCount(count + 1);
    }
  };

  return (
    <div>
      <p>{count}</p>
      <button onClick={incrementFive}>Increment Five</button>
    </div>
  );
};
```

On aura `1` comme résultat !

Pour que cela fonctionne, il faut utiliser la deuxième forme du `setter`, 

### `setValue((oldValue) => newValue)`

```jsx
const incrementFive = () => {
  for (let i = 0; i < 5; i++) {
    setCount(oldCount => oldCount + 1);
  }
};
```

Ici le bouton ajoute bien 5.

### ! si on veut modifier la valeur avec sa valeur précédente, on passe une fonction au `setter`

Modification du composant :

```jsx
const HookCounterTwo = () => {
  const initialCount = 0;
  const [count, setCount] = React.useState(initialCount);

  return (
    <div>
      <p>{count}</p>
      <button onClick={() => setCount(oldCount => oldCount + 1)}>
        Add One
      </button>
      <button onClick={() => setCount(oldCount => oldCount - 1)}>
        Substract One
      </button>
      <button onClick={() => setCount(initialCount)}>Reset</button>
    </div>
  );
};
```

### Modification dans le composant classe

```jsx
addOne = () => {
  this.setState(prevState => {
    return { count: prevState.count + 1 };
  });
};
```

### `this.setState( prevState => return newState )`

## Utilisation d'objet avec `useState`

Première version naïve :

```jsx
const HookForm = () => {
  const [name, setName] = React.useState({ firstName: "", lastName: "" });
  return (
    <div>
      first name :
      <input
        type="text"
        onChange={e => setName({ firstName: e.target.value })}
      />
      &nbsp;&nbsp; last name :
      <input
        type="text"
        onChange={e => setName({ lastName: e.target.value })}
      />
      <br />
      <p>{JSON.stringify(name)}</p>
    </div>
  );
};
```

Quand on rempli `firstName` , `lastName` disparaît et vice versa.

On voit bien que `setName` dans sa forme, écrase l'une ou l'autre de ses propriété.

#### ! spread operator `...`

D'abord voyons comment écraser une valeur de propriété d'un objet littéral :

```js
const tony = {name: "tony", lastName: "Danza", age: 24, lastName: "Princeton"};
 tony; //? { name: 'tony', lastName: 'Princeton', age: 24 } 
```

On observe que `lastName` prend la dernière valeur donnée.

Maintenant fesont la même chose avec l'opérateur de décomposition :

```js
 let tonyTwo = {name: "tony", lastName: "Danza", age: 24};

 tonyTwo = {...tonyTwo, lastName: "Princeton"};
// ...tonyTwo est remplcé par name: "tony", lastName: "Danza", age: 24 et on revient dans le cas de figure précédent

 tonyTwo; //? { name: 'tony', lastName: 'Princeton', age: 24 } 
```

### Revenons à useState

Pour corriger le problème nous allons nous servir de l'opérateur de décomposition :

```jsx
const HookForm = () => {
  const [name, setName] = React.useState({ firstName: "", lastName: "" });
  return (
    <div>
      first name :
      <input
        type="text"
        onChange={e => setName({ ...name, firstName: e.target.value })}
      />
      &nbsp;&nbsp; last name :
      <input
        type="text"
        onChange={e => setName({ ...name, lastName: e.target.value })}
      />
      <br />
      <p>{JSON.stringify(name)}</p>
    </div>
  );
};

```

##### `onChange={e => setName(...name, firstName: e.target.value)}`

On note aussi l'utilisationde `JSON.stringify` pour débugguer dans le template  

## `useState` avec les tableaux

```jsx
const HookArray = () => {
  const [items, setItems] = React.useState([]);

  let value = "";

  const changeValue = (e, item) => {
    items[item.id] = { id: item.id, value: e.target.value };
    console.log(items);
    setItems([...items]);
  };

  return (
    <div>
      <p>
        <button
          onClick={() =>
            setItems([...items, { id: items.length, value: value }])
          }
        >
          add item
        </button>
        <input type="text" onChange={e => (value = e.target.value)} />
      </p>
      <ul>
        {items.map(item => (
          <li key={item.id}>
            {item.value}&nbsp;&nbsp;
            <input type="text" onChange={e => changeValue(e, item)} />
          </li>
        ))}
      </ul>
      {JSON.stringify(items)}
    </div>
  );
};
```

Le résumé de ce code, c'est que pour modifier un tableau dans le state il faut faire :

#### `setItems([...items])`

Pour ajouter un élément :

#### `setItems([...items, { id: items.length, value: "something"}])`

## Résumé

- Le state peut être un nombre, une chaîne de caractère, un booléen, un object ou un tableau
- `useState` renvoie un tableau avec la valeur du `state` et un `setter` pour cette valeur
- Ce `setter` provoque un rendu de la vue
- On peut utiliser une fonction comme argument du `setter` si la nouvelle valeur du `state` dépend de l'ancienne
- Les tableaux et les objets utilisent l'opérateur de décomposition `...` avec le `setter` pour se mettre à jour

