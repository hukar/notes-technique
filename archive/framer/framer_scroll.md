# Framer Scroll Component

```coffeescript
scroll = new ScrollComponent
	size: Screen.size
	scrollHorizontal: false

list.parent = scroll.content
```

`scrollHorizontal: false` contraint le scroll sur l'axe vertical

On peut ajouter une marge à la zone de scroll :

```coffeescript
scroll.contentInset =
	bottom: 0
```

## animation quand on scroll

```coffeescript
scroll.onScroll ->
	if scroll.scrollY > 36
		ball.animate
			y: 34
			options: 
				time: 0.6
				curve: Spring
		
	if scroll.scrollY <= 36
		ball.animate
			y: 0 - ball.height
			options: 
				time: 0.6
				curve: Spring	
```

`onScroll` est l'événement de l'objet scroll

On anime une balle suivant le déplacement sur Y du scroll.

il existe aussi deux autres événement de scroll `onScrollSart` et `onScrollEnd`.

## Problème de la taille de scroll.content

Il faut ajouter un container englobant les éléments du scroll et de lui donner une taille suffisante.

```coffeescript
main = new Layer
	parent: Mobile
	height: 2500 # ici une taille suffisante
	width: Screen.width
	backgroundColor: "rgb(255, 255, 255)"

for elt in eltsScroll
	elt.style.overflow = "hidden"
	elt.style.position = "relative"
	elt.y = 0
	elt.style.marginBottom = "22px"
	elt.parent = main # on met les éléments dans main
	
main.parent = scroll.content # enfin on met main dans scroll.content	
```

De cette façon scroll.content a la bonne taille.