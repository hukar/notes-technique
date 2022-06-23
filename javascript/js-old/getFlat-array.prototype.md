# Aplatir des tableaux

`getFlat` renvoie un tableau aplati de manière récursive :

```js
Array.prototype.getFlat = function() {
  const flatten = []
  
  function deepFlat (arr){
    arr.forEach((item) => {
      if(item instanceof Array) {
        console.log('recursive');
        deepFlat(item);
      } else {
        flatten.push(item);
      }
    })
  }
  
  deepFlat(this);
  
  return flatten;
}
```

entrée:

```json
const exchanges = [
    [{id: 234,name: 'knife',price: 67}, {id: 567,name: 'fork',price: 34}],
    [{id: 963,name: 'spoon',price: 23}, {id: 111,name: 'plate',price: 16}],
  [{id: 566,name: 'bowl',price: 7}, [{id: 342,name: 'chair',price: 219},
                                    [{name: 'toto'},{name: 'pipi'}]]],
];
```

sortie:

```bash
"[{"id":234,"name":"knife","price":67},{"id":567,"name":"fork","price":34},{"id":963,"name":"spoon","price":23},{"id":111,"name":"plate","price":16},{"id":566,"name":"bowl","price":7},{"id":342,"name":"chair","price":219},{"name":"toto"},{"name":"pipi"}]"
```

