# Les chaînes de caractères
### fonction confirmEnding

```javascript
function confirmEnding(str, target) {
  var strTest = str.substring((str.length-target.length));
  return (strTest == target);
}
```

`substring` reçoit l'indice à partir duquel coupé la chaîne

`str.substring(indiceA[, indiceB])`

### une fonction truncate

Couper une chaîne et ajouté ...

```javascript
function truncateString(str, num) {
  var newStr = "";
  if (num > 3) {
    newStr = (num < str.length)?(str.substring(0,num - 3) + "..."):str;
  } else {
    newStr = str.substring(0,num)+"...";
  }
  return newStr; 
}
  
  truncateString("A-tisket a-tasket A green and yellow basket", 11);
  > "A-tisket..."
}
```

num est la longueur avec les trois petits points compris (d'où le __num - 3__ en deuxième paramètre)