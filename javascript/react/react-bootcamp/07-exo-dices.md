# 07 Exercice Roll Dice

## Utilisation De Font Awesome

Téléchargement des fichiers : utilisation de la version `js` 

C'est le fichier `all.js`, il transforme les balise `<i>`  en `<svg>`

Dans index.

```html
<script defer src="all.js"></script>
```

#### MDN :

Les scripts qui utilisent l'attribut `defer` empêche le déclenchement de l'évènement `DOMContentLoaded` tant que le script n'a pas été chargé et que son évaluation n'est pas terminée.

#### Problème avec le js pour les rendus

Du coup, j'ai utilisé `all.css`. Il faut ajouter le dossier `webfonts`.

### imrc => import react component

Snippet 

```jsx
// imrc + tab
import React, { Component } from 'react'
```

## Flexbox

```css
.parent {
  display: flex;
}

.child {
  align-self: center;
}
```

## Syntaxe alternative

Au lieu d'écrire un Ternaire dans un `className`, si on a pas de valeur alternative on peut utiliser le `&&`.

```jsx
<i
  className={`fa fa-dice-${this.props.face}${this.props
    .shake && " Die-shake"}`}
  ></i>
```

au lieu de :

```jsx
<i
  className={`fa fa-dice-${this.props.face}${
  this.props.shake ? " Die-shake" : "" }`}
  ></i>
```

Cette syntaxe ne fonctionne pas avec un composant fonctionnel.

## Hook state

Si on a pas besoin du `setter` , on peut encore simplifier l'écriture du `hook` :

```jsx
const [numberOfDices] = useState(3);
```

