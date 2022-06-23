# 10 Pre-évenir le comportement par défaut

## En `HTML`

On peut utiliser `return false;`

```html
<a href="#" onclick="console.log('Le lien a été cliqué.'); return false">
  Clique ici
</a>
```

En `React` on utilise `evt.preventDefault()`.

C'est une version synthétique propre à `React` et compatible avec tous les 

navigateurs.

```jsx
function Link() {
    const handleClick = e => {
        e.preventDefault();
        console.log({ ...e });
      
      // on y retourne quand même :)
        window.location = e.target.href;
    };

    return (
        <a
            className="btn btn-outline-success"
            href="http://www.google.com"
            onClick={handleClick}
        >
            Google go!!
        </a>
    );
}
```

#### ! l'objet `e` a tous ses attributs mis à `null` par `React` après utilisation.

#### ! Pour pouvoir accéder à ses attributs il faut faire une copie `{ ...e }`