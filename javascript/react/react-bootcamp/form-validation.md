# Validation de formulaire

## Composant Input

```jsx
function Input(props) {
  let inputElement = null;

  switch (props.inputtype) {
    case "input":
      inputElement = <input {...props}   // on dévellope tous les attributs
      break;
    case "textarea":
      inputElement = <textarea {...props} />;
      break;
    default:
      inputElement = <input {...props} />;
  }

  return (
    <div>
      <label>{props.label}</label>
      {inputElement}
    </div>
  );
}
```

Dans le composant parent :

```jsx
<Input
  name="pwd"
  inputtype="input"
  type="password"
  placeholder="password"
  label="password :"
  />
<Input
  name="text"
  inputtype="textarea"
  placeholder="text for describe something ..."
  label="text :"
  />
```

`{...props}` permet de deployer tous les attributs, même si on ne connait ni leurs nombres ni leurs types.

### ! `<texarea />`  est une balise auto-fermante dans React

#### ! les attributs sont en minuscule

## Formulaire généré automatiquement

On crée d'abord dans le `state` un objet représentant le formulaire :

```jsx
import uuid from "uuid/v4";
```

Pour avoir une valeyr de `key` unique. (`npm install uuid`)

```jsx
this.state = {
      orderForm: {
        name: {
          id: uuid(),
          elementType: "input",
          elementConfig: {
            type: "text",
            placeholder: "your name :"
          },
          value: ""
        },
        email: {
          id: uuid(),
          elementType: "input",
          elementConfig: {
            type: "email",
            placeholder: "your email :"
          },
          value: ""
        },
        deliveryMethod: {
          id: uuid(),
          elementType: "select",
          elementConfig: {
            options: [
              { value: "fastest", displayValue: "Fastest" },
              { value: "cheapest", displayValue: "Cheapest" }
            ]
          },
          value: ""
        }
      }
    };
```

Pour générer le formulaire on utilise une boucle `for (key in object)` :

```jsx
let inputs = [];

    for (let item in this.state.orderForm) {
      inputs.push(
        <Input
          key={this.state.orderForm[item].id}
          name={item}
          elementType={this.state.orderForm[item].elementType}
          elementConfig={this.state.orderForm[item].elementConfig}
          value={this.state.orderForm[item].value}
          stateChange={this.stateChange}
        />
      );
    }
```

```jsx
return (
  <div>
    <form onSubmit={this.handleSubmit}>
      {inputs}
      <button>Envoyer !!</button>
    </form>
  </div>
);
```

### Le composant `Input`



```jsx
import React from "react";
import uuid from "uuid/v4";

function Input(props) {
  let inputElement = null;

  function handleChange(evt) {
    const { name, value } = evt.target;
    props.stateChange(name, value);
  }

  switch (props.elementType) {
    case "input":
      inputElement = (
        <input
          name={props.name}
          {...props.elementConfig}
          value={props.value}
          onChange={handleChange}
        />
      );
      break;
    case "textarea":
      inputElement = (
        <textarea
          name={props.name}
          {...props.elementConfig}
          value={props.value}
          onChange={handleChange}
        />
      );
      break;
    case "select":
      inputElement = (
        <select onChange={handleChange} name={props.name} value={props.value}>
          {props.elementConfig.options.map(option => (
            <option key={uuid()} value={option.value}>
              {option.displayValue}
            </option>
          ))}
        </select>
      );
      break;
    default:
      inputElement = (
        <input name={props.name} {...props.elementConfig} value={props.value} />
      );
  }

  return {inputElement};
}
```

#### ! bien mettre `value` sur chaque `option` mais aussi sur `select`

### Zoom sur l'élément `select`

```jsx
case "select":
      inputElement = (
        <select onChange={handleChange} name={props.name}>
          <option value=""> --- </option>
          {props.elementConfig.options.map(option => (
            <option key={uuid()} value={option.value}>
              {option.displayValue}
            </option>
          ))}
        </select>
      );
```

On crée un `map ` sur le tableau d'options reçu dans `props.elementConfig.options`

Le `onChange` est sur la balise `<select>`

### Focus sur le flux ascendant des données

```jsx
function handleChange(evt) {
  const { name, value } = evt.target;
  props.stateChange(name, value);
}
```

En fait ici on récupère la référence vers la fonction `stateChange` du parent par les propriété `props`

On récupère par décomposition de l'objet `evt.target` `name` et `value`

### Dans l'élément parent (où le `state` est géré)

```jsx
stateChange(name, value) {
  this.setState(prevState => {
    let newForm = JSON.parse(JSON.stringify(prevState.orderForm));
    newForm[name].value = value;

    return { orderForm: newForm };
  });
}
```

Pour faire une copie profonde de l'objet `orderForm` :

```jsx
let newForm = JSON.parse(JSON.stringify(prevState.orderForm));
```

#### gérer la soumission de formulaire

Dans le template

```jsx
<form onSubmit={this.handleSubmit}>
```

La fonction en elle-même :

```jsx
handleSubmit(evt) {
  evt.preventDefault();
  console.log(this.state);
}
```

## La Validation en elle-même

### L'objet formulaire

```jsx
this.state = {
      orderForm: {
        name: {
          id: uuid(),
          elementType: "input",
          elementConfig: {
            type: "text",
            placeholder: "your name :"
          },
          value: "",
          validation: {
            required: true,
            maxLength: 5,
            minLength: 3
          },
          valid: false
        }
      }
    };
```

`validation` va prendre toutes les règles de validation

`valid` correspond à l'état du composant : `valide` ou `non valide`

### La méthode de validation

```jsx
checkValidity(value, rules) {
  let isRequired,
      hasMinLength,
      hasMaxLength = false;

  if (rules.required) {
    isRequired = value.trim() !== "";
  }

  if (rules.minLength) {
    hasMinLength = value.trim().length >= rules.minLength;
  }

  if (rules.maxLength) {
    hasMaxLength = value.trim().length <= rules.maxLength;
  }
  return isRequired && hasMinLength && hasMaxLength;
}
```

Crée une variable pour chaque règle et renvoyer leurs prédicat me semble une façon assez lisible.

### Dans une méthode de mise à jour

```jsx
newForm[name].valid = this.checkValidity(value, newForm[name].validation);
```

`checkValidity` renvoie un booléen.

### ! Rappel javascript

`statement` faire quelque chose

`expression` renvoyer une valeur

```js
a = b;
```

C'est à la fois un `statement` et une `expression` qui vaut b.

```js
a = b = c;
// peut s'écrire 
a = (b = c);
// (b = c) a pour valeur c donc:
a = c;
```

#### ! une déclaration en série doit utiliser des virgules

Mauvais exemple :

```js
const a = b = c = true;
a; //? true
b; //? true
c; //? true

b = 4; //? 4
c = 4; //? 4
a = 4; //? error : Assignment to constant variable.
```

`b` et `c` sont donc deux variables globales !!

l'écriture juste :

```js
const a = true, b = true, c = true;
```

## Affichage de la validation

On doit ajouter la notion de `touched` car un formulaire vierge ne peut pas déjà montrer des erreurs de validation :

```jsx
email: {
         // ...,
          validation: {
            required: true
          },
          valid: false,
          touched: false
        },
```

Ensuite on pass `touched` à `true` dès que l'élément est touché, c'est à dire `onChange` :

```jsx
stateChange(name, value) {
    this.setState(prevState => {
      // ...
      newForm[name].touched = true;
			// ...
    });
  }
```

Puis on passe `touched` par les `props` :

```jsx
<Input
          // ...
          touched={this.state.orderForm[item].touched}
          invalid={!this.state.orderForm[item].valid}
        />
```

Enfin la condition sur la classe **CSS** tient en compte `touched` dans le composant `Input`:

```jsx
<input 
  className={props.invalid && props.touched ? "red" : "green"}
  // ...
```

### Ajouter un message d'erreur

Dans le composant `Input` :

```jsx
{props.invalid && props.touched && <p className="red">Pas Valide</p>}
```

Bien sure le message peut-être passé aux `props` :

```jsx
{props.invalid && props.touched && <p className="red">{props.messageError}</p>}
```

## Validation du formulaire entier

Création d'une méthode simple :

```jsx
checkFormValidity(form) {
    for (let key in form) {
      if (!form[key].valid) {
        return false;
      }
    }

    return true;
  }
```

Si cette méthode trouve un élément non valide elle renvoit `false` sinon elle renvoie `true`

Dans le template on `disabled` le bouton :

```jsx
<form onSubmit={this.handleSubmit}>
  {inputs}
  <button disabled={!this.checkFormValidity(this.state.orderForm)}>
    Envoyer !!
  </button>
</form>
```

On peut bien sure créer un nouvel attribut dans le `state` `formIsValid` ou autre.

