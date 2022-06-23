# Framer flow

## Pagination avec FlowComponent

```coffeescript
flow = new FlowComponent
flow.showNext(screen1)

btnGo.onClick ->
	flow.showNext(screen2)

btnGoTwo.onClick ->
	flow.showNext(screen3)
	
back1.onClick ->
	flow.showPrevious()

back2.onClick -> 
	flow.showPrevious()
```

FlowComponent se souviens de l'écran précédent et on peut utiliser ```flow.showPrevious()``` sans argument.

## Overlay/Modal et menu hamburger

```coffeescript
# menu hamburger
flow = new FlowComponent

flow.showNext(screen1)

btnMenu.onClick ->
	flow.showOverlayLeft(lateralMenu)
```

Bouton pour revenir en arrière :

```coffeescript
btnCloseMenu.onClick ->
	flow.showPrevious()
```

### Corriger la hauteur du menu au resize

```coffeescript
lateralMenu.height = Screen.height
```

Screen étant l'écran d'affichage