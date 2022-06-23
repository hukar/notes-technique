# framer Animate

```coffeescript
	it.children[0].onClick () ->
		print "click"
		@.parent.animate
			height: 0
			options:
				time: 0.6
				curve: Spring
		@.parent.onAnimationEnd () ->
			@.destroy()
```

syntaxe `layer.animate`

`@` équivaut à `this`

`onAnimationEnd` permet de ne pas détruire l'élément avant que son animation ne se soit produite