# 03 Props

`index.js`

```jsx
class App extends React.Component {
  render() {
    return (
      <div>
        <Hello from="boris" to="romain" />
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("my-app"));
```

Ici `from` et `to` sont des chaînes de caractères, on peut donc utiliser les `"string"` guillemets doubles.

`Hello.js`

```jsx
function Hello({ from, to }) {
  return (
    <div>
      <p>Hello {to}</p>
      <p>It's {from}</p>
    </div>
  );
}
```

`{ from, to }` récupère grace à l'assignation par décomposition les valeurs de `props.from`  et `props.to`

```js
const props = {
    to: "romain",
    from: "boris"
};

const { to, from } = props;
to; //? romain
from; //? boris
```

 ### La même chose avec une classe

```jsx
class Salut extends React.Component {
  render() {
    const { to, from } = this.props;
    return (
      <div>
        <p>
          a message to {to} from {from}
        </p>
      </div>
    );
  }
}
```

La classe `Salut`  recevant automatiquement l'objet `props`.

#### Les propriétés servent à configurer votre composant

#### Les propriétés sont immutable (immuable)

```jsx
class Salut extends React.Component {
  render() {
    this.props.from = "Daisy"; // not possible, this.props is immutable
    return (
      <div>
        <p>
          a message to {this.props.to} from {this.props.from}
        </p>
      </div>
    );
  }
}

// Uncaught TypeError: Cannot assign to read only property 'from' of object '#<Object>'
```

On ne peut pas nom plus étendre `this.props` :

```jsx
class Salut extends React.Component {
  render() {
    this.props.age = 10;
// Uncaught TypeError: Cannot add property age, object is not extensible
    
    return ( /* ... */ );
  }
}
```

## Les types de propriété

Il y a trois syntax pour les propriété :

### `prop="something"`

C'est une propriété de type `string`

### `prop={ ... }`

Avec les accolades, on peut passer n'importe quel type javascript, les accolades acceptent une expression javascript.

### `prop`

Est équivalent à `prop={true}`

#### exemples

```jsx
class App extends React.Component {
  render() {
    return (
      <div>
        <Hello
          from="boris"
          to="romain"
          age={36}
          notes={[12, 16, 3, 11]}
          pet={{ name: "pussy", age: 5, specie: "dog" }}
          isFunny // idem isFunny={true}
          bangs={6}
          urlImg="https://i.ytimg.com/vi/cKX9vbCHQrk/maxresdefault.jpg"
        />
      </div>
    );
  }
}

ReactDOM.render(<App />, document.getElementById("my-app"));
```

Voici l'utilisation dans le composant :

```jsx
function Hello(props) {
  const { from, to, bangs, urlImg } = props;
  return (
    <div>
      <p>
        Hello {to} I'm {from} {"!".repeat(bangs)}
      </p>
      <p>Props : {JSON.stringify(props)}</p>

      <img src={urlImg} />
    </div>
  );
}
```

#### ! `"string".repeat(n)`  va répéter la chaîne de caractère `n`  fois.

![image-20191015155628563](assets/image-20191015155628563.png)

Voici le résultat à l'écran.

## Valeurs par défaut

Si certaine propriété ne sont pas définies, on peut en définir par défaut avec `static defaultProps` :

`Index.js`

```jsx
class App extends React.Component {
  render() {
    return (
      <div>
        <Hello from="Maurice" to="Elise" bangs={5} />
        <Hello to="Malavra" bangs={10} />
        <Hello to="Carolina" />
      </div>
    );
  }
}
```

On voie que certaines propriété n'ont pas de valeur.

`hello.js`

```jsx
class Hello extends React.Component {
  static defaultProps = {
    from: "Anonymous",
    bangs: 1
  };
  render() {
    const { from, to, bangs } = this.props;
    const b = "!".repeat(bangs);
    return (
      <div>
        <p>
          Hi {to} from {from} {b}
        </p>
      </div>
    );
  }
}
```

Voici en bas la partie importante :

```jsx
static defaultProps = {
  from: "Anonymous",
  bangs: 1
};
```

## `className` et `htmlFor`

### `htmlFor`

`for` étant un mot réservé, il est remplacé en **JSX** par `htmlFor`

```jsx
<p>
  <label htmlFor="name">Name : </label>
  <br />
  <input type="text" id="name" />
</p>
```

### className

De même le mot `class` étant réservé, on utilise en **JSX** `className`

```jsx
<p className="Machine">Hello Animals</p>
```

### Style dynamique avec la propriété `className`

Dans un fichier `css`

```css
.purple {
  color: white;
}
.orange {
  color: white;
}
```

En **JSX** :

```jsx
const coin = Math.floor(Math.random() * 2 + 1) === 2;
// ...
<p className={coin ? "orange" : "purple"}>Hello Animals</p>
```

### Style dynamique avec la propriété `style`

On peut passer un **objet javascript** à la propriété `style`, l'écriture des propriétés `css` passe du `kebab-case` au `camelCase`

```jsx
const myStyle = {
  fontSize: "22px",
  color: "purple"
};

const pinkStyle = {
  backgroundColor: "pink"
};

const greenStyle = {
  backgroundColor: "lightgreen"
};

// ...

<ul style={myStyle}>
  {animals.map(a => (
    <li style={coin ? pinkStyle : greenStyle} key={a}>
      {a}
    </li>
  ))}
</ul>
```

Les propriétés css ont des valeurs entres guillemets (chaînes de caractères) et sont séparées par des virgules.

### Syntaxe Javascript

####  `Array.from({length: 6}, (v,k) => k)`

Permet de générer une séquence de nombre

```js
Array.from({length: 6})
// [ undefined, undefined, undefined, undefined, undefined, undefined ]
[...Array(6)]
// [ undefined, undefined, undefined, undefined, undefined, undefined ]
```

