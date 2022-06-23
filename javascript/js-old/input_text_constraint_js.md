# Contraindre un champs texte

## limiter l'entrée aux nombres

```html
<form action="">
	<input id="date" type="text" placeholder="00" size="2"/>
</form>
```

### premièrement :

Dans le javascript si l'événement onkeypress renvoie false rien n'est écrit

```js
document.getElementById('date')

picker.onkeypress = function(){
  return false
}
```
Ainsi aucune touche n'entre de valeur.

Pour laisser seulement les chiffres

```js
picker.onkeypress = function(evt){
  return evt.key.search(/[0-9]/) != -1
}
```
`evt.key` est de type string, on utilise donc `search`   
*(String.search(regex) -1 si rien trouvé sinon l'indice de la première correspondance)*


### deuxièmement il faut empécher les copy-paste

```js
picker.onpaste = function(evt) {
  return false
}
```
### Petit bonus : récupérer le presse-papier (clipboard)

```js
evt.clipboardData.getData("text")
// renvoie le texte du copier
```