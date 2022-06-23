# 03 Introduction à `React`

`React` utilise un `Virtual DOM` pour manipuler le `DOM` de manière ciblée.

Il ne modifie le `DOM` qu'au endroits strictement nécessaires.

Si on manipule soit même le `DOM`, tout le `HTML` est modifié :

```html
<div id="html-clock">Hello</div>

<script>
  const clock = document.getElementById("html-clock");
  const render = () => {
    clock.innerHTML = `
	<h3>SuperClock</h3>
	<p><input /></p>
	<p>${new Date()}</p>
	`
  }

  setInterval(render, 1000)
</script>
```

ici, il est impossible de rentrer une donné dans le `input` car celui-ci est réécrit toutes les secondes.



## Utiliser `React` pour résoudre ce problème

```js
function App() {
  const [myDate, myDateUpdate] = useState(new Date())
  useEffect(() => {
    const intervalId = setInterval(() => {
      myDateUpdate(new Date())
    }, 1000)
    
    return () => cleanInterval(intervalId)
  })

  return (
    <div>
   	  <h3>My Clock</h3>
    	<p><input type="text" /></p>
    	<p>{ myDate.toString() }</p>
		</div>
);
}
```

Si la fonction à des effets de bord (modifier l'affichage de la date toute les secondes), on utilise le `hook` : `useEffect`.

On renvoie une fonction de nettoyage.

`React` est assez intéllignet pour ne modifier que la partie changeante du  `HTML`.

