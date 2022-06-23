# States dans Framer

ajouter un `states`:

```coffeescript
layer.states.a =
		y: 13
		x: 37
		options:
			time: 0.60
			curve: Spring
```

animer un `states`

```coffeescript
layer.animate("a")
```

créer un cycle d'animation `layer.animate(properties or state, options)`

```coffeescript
layer.states =
	a:
		y: 13
	b:
		y: 63
	c:
		y: 112
	
layerAnimationOptions =
	time: 0.60
	curve: Spring

btn.onClick (event, layer) ->
	layer.stateCycle(["b","c","a"], layerAnimationOptions)
```

Deuxième syntaxe sans les options :

```coffeescript
btnLanguage.onClick (event, layer) ->
	language.stateCycle("indoor", "outdoor")
```

Autre syntaxe en utilisant `layer.animationOptions` :

```coffeescript
layer.states =
	a:
		y: 13
	b:
		y: 63
	c:
		y: 112
	
layer.animationOptions =
	curve: Spring
	time: 0.60
	
btn.onClick ->
	layer.stateCycle("b", "c", "a")
```

