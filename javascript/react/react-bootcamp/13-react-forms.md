#  13 Les formulaires en React

## Controlled Component

### One Source of Truth

Le `state` de React sera la seule source de vérité.

On garde le formulaire sous contrôle en *bindant* l'attribut `value` avec le `state`

```jsx
class Form extends Component {
  constructor(props) {
    super(props);

    this.state = {
      username: ""
    };

    this.handleChange = this.handleChange.bind(this);
    this.handleSubmit = this.handleSubmit.bind(this);
  }

  handleChange(evt) {
    this.setState({ username: evt.target.value });
  }

  handleSubmit(evt) {
    evt.preventDefault();
    console.log(`username : ${this.state.username}`);

    this.setState({ username: "" });
  }

  render() {
    return (
      <div className="Form">
        <form onSubmit={this.handleSubmit}>
          <input
            type="text"
            value={this.state.username}
            onChange={this.handleChange}
          />
        </form>
      </div>
    );
  }
}
```

`evt.preventDefault()` evite que le navigateur se recharge à chaque fois que le formulaire est soumis.

`evt.stopPropagation()` évite que l'événement ne se propage dans le DOM.

#### ! Comme il n'y a qu'un seul `input` texte, le formulaire se soumet lorsqu'on click sur entrée dans le champs texte: pas besoin d'un bouton `submit`  

## Gérer plusieurs `inputs`

### Première approche

```jsx
class NewForm extends Component {
  constructor(props) {
    super(props);
    this.state = {
      name: "",
      tel: ""
    };
    this.handleForm = this.handleForm.bind(this);
    this.handleName = this.handleName.bind(this);
    this.handleTel = this.handleTel.bind(this);
  }

  handleName(evt) {
    this.setState({
      name: evt.target.value
    });
  }
  handleTel(evt) {
    this.setState({
      tel: evt.target.value
    });
  }

  handleForm(evt) {
    // ...
  }

  render() {
    return (
      <div className="NewForm" onSubmit={this.handleForm}>
        <form>
          <input value={this.state.name} onChange={this.handleName}/>
          <input value={this.state.tel} onChange={this.handleTel} />

          <button>SUBMIT THE FORM</button>
        </form>
      </div>
    );
  }
}
```



### Nouveauté ES2015

Clé d'objets dynamique basée sur les expressions Javascript

**Computed property name**

```js
const codeid= 67543244566009;

const titi = {
    [codeid]: "my secret"
};

titi; //? { 67543244566009: 'my secret' }
```

Un autre exemple :

```js
const computation = (5*6 - 7) / 9;

const titi = {
    [computation]: "my secret"
};

titi; //? { '2.5555555555555554': 'my secret' }
```

On peut alors simplifier l'écriture de notre composant :

```jsx
handleChange(evt) {
  this.setState({
    [evt.target.name]: evt.target.value
  });
}

// ...
<input
  value={this.state.name}
  name="name"
  onChange={this.handleChange}
  />

<input
  value={this.state.tel}
  name="tel"
  onChange={this.handleChange}
  />
```

#### Amélioration de la syntaxe avec la l'affectation par décomposition :

```jsx
handleChange(evt) {
  const {name, value} = evt.target;
  this.setState({
    [name]: value
  });
}
```



### `htmlFor`

En `html` classique l'attribut `for` permet de relier une étiquette à un élément de formulaire.

Comme en `jsx`, `for` est un mot clé, on utilise `htmlFor` à la place.

```jsx
<label htmlFor="name">le nom :</label>

<input
  id="name"
  value={this.state.name}
  onChange={this.handleChange}
  />
```

`htmlFor` fonctionne avec l'`id` d'un élément.

## Rappel

### `slice`

renvoie une copie superficielle (shallow copy), le tableau originel n'est pas modifié

```js
slice(begin, end) // begin inclus, end exclus
```

Supprimer un élément d'indice idx d'une liste par copie :

```js
let ls = [..."abcdef"];

let idx = 3;

const newList = [...ls.slice(0,idx),...ls.slice(idx + 1)];
```

```js
[ 'a', 'b', 'c', 'e', 'f' ]
```

### `findIndex`

Permet de retrouver l'indice du premier vérifiant condition donnée par une fonction :

```js
const list = [
    {name: "titi", qty: 8},
    {name: "toto", qty: 5},
    {name: "tata", qty: 3}
];

let index = list.findIndex(x => x.name === "toto"); //? 1
```

## UUID library

Pour avoir des clés unique Universally Unique IDentifier

```bash
npm install uuid
```

Utilisation :

```jsx
import uuid from "uuid/v4";

class ShoppingListForm extends Component {
  constructor(props) {
    super(props);

    this.state = {
      name: "",
      qty: "",
      id: uuid()
    };

// ...
```

