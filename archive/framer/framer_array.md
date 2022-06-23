# framer array

## génération d'item avec un array

```coffeescript
itemHeight = item.height
itemY = item.y

items = []

for i in [1..35]
	itemClone = item.copy()
	itemClone.name = "clone"
	itemClone.parent = scroll.content
	itemClone.opacity = item.opacity - 0.2*(i%4)
	
	items.push(itemClone)
showItems = () ->	
	for elt, i in items
		elt.y = i*12 + i*itemHeight + itemY
		
showItems()
```

On a une `item` de base.

on la clone avec `item.copy()`

On crée un tableau `items = []` et on ajoute un élément avec `items.push(itemClone)`