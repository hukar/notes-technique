# 11 exercice : Hangman

## Version du formateur

### Pré-chargement des images

```jsx
import img0 from "./0.jpg";
import img1 from "./1.jpg";
import img2 from "./2.jpg";
...
```

les images vont de 0 à 6 pour être appelées par l'index d'un tableau.

```jsx
 static defaultProps = {
    maxWrong: 6,
    images: [img0, img1, img2, img3, img4, img5, img6]
  };
```

Voici donc le tableau où `imgn` contient l'image de l'import `"./n.jpg"`

Le state est assez petit :

```jsx
this.state = { nWrong: 0, guessed: new Set(), answer: "apple" };
```

`nWrong` : le nombre de faute

`Guessed` : les lettres devinées c'est un Set