# Compositon

Pouvoir passer une fonction à une fonction

sans la composition :

```js
var animals = [{
    "name": "Ileana",
    "species": "Lion"
  }, {
    "name": "Kathrine",
    "species": "Peguin"
  }, {
    "name": "Avery",
    "species": "Butterfly"
  }]

var penguinTab = []
for( animal of animals) {
    
    if(animal.species === 'Penguin')
        penguinTab.push(animal)
}
```

avec la composition de fonction :

```js

var isPenguin = animal => animal.species === 'Penguin'

var penguinClan = animals.filter(isPenguin)
```

