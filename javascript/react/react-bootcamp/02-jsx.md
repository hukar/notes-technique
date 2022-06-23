# 02 JSX : javascript + XML

 	

## Règle JSX

Un composant ne peut retourner que **un** et **un seule** élément

```jsx
function Hello({name}) {
    return (
    <h1>Hello {name}</h1>
    <p>One story about root element</p>
    );
}
```

```bash
error: Adjacent JSX elements must be wrapped in an enclosing tag
```

```jsx
function Hello({ name }) {
  return (
    <div>
      <h1>Hello {name}</h1>
      <p>One story about root element</p>
    </div>
  );
}
```

Maintenant cela fonctionne !

## JSX : Sucre Syntaxique produit par Babel

On peut utiliser l'outil en ligne de Babel pour transpiler son JSX en javascript.

### jsx

```jsx
<h1 class="title" onClick="doSomething" onLoad={DoSomethingElse}>
  hello <b>Coco</b>
</h1>
```

### javascript

```js
React.createElement("h1", {
  class: "title",
  onClick: "doSomething",
  onLoad: DoSomethingElse
}, "hello ", React.createElement("b", null, "Coco"));
```

`React.createElement(elementName, props, nodeText, htmlChildren )`

Dans les `props` on remarque que les guillemets passe une chaine de caractères, alors que les accolades passe directement le nom d'une variable.



## Utilisation des expressions en JSX `{ ... }`


```jsx
function getMood() {
  const moods = ["Angry", "Hungry", "Quiet", "Paranoïd", "Silly"];

  return moods[Math.floor(Math.random() * moods.length)];
}

function Hello({ name }) {
  return (
    <div>
      <h3>My current mood is : {getMood()}</h3>
    </div>
  );
}
```

## Les conditions

> #### ! Rappel javascript
>
> Quelle différence entre `Math.floor(Math.random() * 4 + 1)` 
>
> et `Math.ceil(Math.random() * 4)` ??
>
> En fait le premier est bien dans l'interval [1, 10], alors que comme `Math.random()` peut retourner `0` (même si la fréquence est faible), le deuxième est donc dans l'interval [0, 10].  
>
> ```js
> function ceilRandom() {
>     return Math.ceil(Math.random() * 4);
> }
> 
> function floorRandom() {
>     return Math.floor(Math.random() * 4 + 1);
> }
> 
> function repartition(f) {
>     const t = new Array(5).fill(0);
>     let nb;
> 
>     for(let i = 0; i < 10*1000*1000; i++) {
>         nb = f();
>         t[nb]++;
>     }
> 
>     return t;
> }
> 
> const repartitionCeil = repartition(ceilRandom);
> const repartitionFloor = repartition(floorRandom);
> 
> repartitionCeil; //? [ 0, 2504385, 2499047, 2498783, 2497785 ]
> 
> repartitionFloor; //? [ 0, 2504385, 2499047, 2498783, 2497785 ]
> ```
>
> ![giphy](assets/giphy.webp)En pratique, 0 ne sort jamais. 

### Première option : l'opérateur ternaire

```jsx
function Conditionnal({ num }) {
  return (
    <div>
      <p>The number is {num}</p>
      {num === 7 ? <p>You win</p> : <p>Hououuuu !!</p>}
      {num === 7 ? <img src="giphy.webp" /> : null}
    </div>
  );
}
```

Pour la deuxième ligne, on peut tirer avantage de l'opérateur `&&` car si le premier postulat est faut il ne regarde pas la partie après le `&&`

```jsx
{num === 7 && <img src="giphy.webp" />}
```

Plus besoin de `: null` cela simplifie un peu la syntax.

> #### ! rappel js
>
> On ne peut pas déclarer une `const` sans l'initialiser :
>
> ```js
> const titi;
> // Uncaught SyntaxError: Missing initializer in const declaration
> ```

### Deuxième option

On utilise une variable dans un `if-else` classique

```jsx
function IfElse({ num }) {
  let msg;

  if (num === 3) {
    msg = (
      <div>
        <p>you win</p>
        <br />
        <img src="giphy.webp" />
      </div>
    );
  } else {
    msg = <p>You loose (-_-')</p>;
  }

  return (
    <div>
      <p> the number is {num}</p>
      {msg}
    </div>
  );
}
```

### ! jsx

Les commentaires :

```jsx
{/* <Conditionnal num={getNum()} /> */}
```

`{/* ... */}`

## Layout

1 composant par fichier `class Hello extends React.Component` -> `Hello.js`

### Composant `App`

C'est le point d'entrée de tous les composants

C'est le composant qui appelle les autres composants.

### `index.js`

`Index.js` a pour seul rôle de rendre `App.js`

```jsx
class App extends React.Component {
  render() {
    return (
      <div>
        <Hello name={"coco"} />
        {/* <Conditionnal num={getNum()} /> */}
        <IfElse num={getNum()} />
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("my-app"));
```

L'objet `ReactDOM` va faire le rendu de `<App />`

## Les boucles

On utilise `map`

`index.js`

```jsx
class App extends React.Component {
  render() {
    return (
      <div>
        <Machine animals={["🐒", "🦅", "🐌", "🐬"]} />
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("my-app"));
```

`Machine.js`

```jsx
const Machine = ({ animals }) => {
  return (
    <div>
      <p>Hello Animals</p>
      <ul>
        {animals.map(a => (
          <li>{a}</li>
        ))}
      </ul>
    </div>
  );
};
```

