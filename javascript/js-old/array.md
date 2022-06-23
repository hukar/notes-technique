# Les tableaux
### pop push shift et unshift

* arr.pop() retire le dernier élément
* arr.push("hello") ajoute un élément à la fin du tableau
* arr.shift() retire le premier élément
* arr.unshift("ola") ajoute un élément au début


### la méthode slice

`var tranche = arr.slice(indiceDebut,indiceFinExclut)`

exemple de fonction qui découpe un tableau en plus petits tableaux de taille = `size`

```javascript
function chunkArrayInGroups(arr, size) {
 var output = [];
  var debut = 0,fin = size;
  while (debut < arr.length) {
    output.push(arr.slice(debut,fin)); 
    debut += size;
    fin += size;
  }
  return output;
}

chunkArrayInGroups(["a", "b", "c", "d","e"], 2);
> [["a", "b"]["c", "d"]["e"]]
```

Une fonction qui retire les premiers éléments (howMany) d'un tableau

```javascript
function slasher(arr, howMany) {
  var output = [];
  
  output = arr.slice((howMany),arr.length);
  return output;
}

slasher([1, 2, 3], 2);
> [3]
```

### Utilistation de indexOf(elt)

indexOf retourne l'indice du tableau correspondant ou -1 s'il ne trouve pas l'élément

```javascript
function mutation(arr) {
  var primStr = arr[0].toLowerCase();
  var secStr = arr[1].toLowerCase();
  var flag = true;

  for (var i = 0;i < secStr.length;i++) {

   if ((primStr.indexOf(secStr[i])) == -1) {
     flag = false;
     break;
   }
  }

  return flag;
}
```

dans cet exemple on considère une chaîne de caractère comme un tableau de caractère : **secStr[i] -> secStr.charAt(i)** équivalent

Tout simplement rechercher l'indice d'un élément :

```javascript
var tab = ["poire","pomme","fraise"]
tab.indexOf("coco")
> -1
tab.indexOf("fraise)
> 2

```

### retirer les éléments False d'un tableau avec Array.prototype.filter

filter(functCallback[,contexte])  retire les élément du tableau générant False en retour de la fonction de callback

```javascript
function bouncer(arr) {

  function test(element) {
   return element;
  }

	arr = arr.filter(test);
  return arr;
}

bouncer([7, "ate", "", false, 9]);
> [7, "ate", 9]
```

### retirer des éléments passés en paramètre

Utilistation d'une closure pour garder une valeur liée à la boucle

```javascript
function destroyer(arr) {
  for (var i = 1;i < arguments.length;i++) {
    
    (function (value) {
       function test (element) {
       return element != value;
	    }
	    arr = arr.filter(test);
    })(arguments[i]);
  
  }
  return arr;
}

destroyer([1, 2, 3, 1, 2, 3],2,1);
> [3, 3]
```

### sort(compareFunc) et l'utilisation de label avec break

`array.prototype.sort([compareFunc])`
De base sort va comparer le code unicode des caractère on a donc :

```javascript
[1,2,14,23].sort()
> [1, 14, 2, 23]
```
Pour obtenir le classement numérique il faut utiliser la fonction en argument optionnel

```javascript
[1,2,14,23].sort(function (a,b) {return a-b;})
> [1, 2, 14, 23]
```

Une fonction qui retourne l'indice où intégrer un élément 

```javascript
function getIndexToIns(arr, num) {
  var ind = 0;
  // on trie d'abord notre tableau
  arr.sort(function (a,b) {
    return a-b;
  });
  etiquette : {
  	// on regarde déjà si num n'est pas plus grand ou égal que le dernier élément
    if (arr[arr.length-1] <= num) {
      ind = arr.length;
      // pas besoin de continuer
      break etiquette;
    }
     for (var i = 0;i < arr.length;i++) {
      if (num > arr[i] && num <= arr[i+1]) {
       ind = i + 1;
       break etiquette;
       }
    }
  }
   
  return ind;
}

getIndexToIns([2, 5, 10], 15);
> 3
```

label : { ...;break label; } break avec un block d'instruction

### Fonction de cryptage rot13 avec String.fromCharCode() et charCodeAt(i)

```javascript
function rot13(str) {
  var neoStr = "";
  for (var i = 0; i < str.length;i++) {

    if (str.charCodeAt(i) > 64 && str.charCodeAt(i) <= 90) {
      if ((str.charCodeAt(i)-65-13) < 0) {
        neoStr += String.fromCharCode(90 + (str.charCodeAt(i)-64-13));
      } else {
        neoStr += String.fromCharCode(str.charCodeAt(i)-13);
      }
    }
    else {
      neoStr += str[i];
    }
  }
  return neoStr;
} 
// les lettres majuscules sont entre 65 -> A et 90 -> Z
// on décale de 13 caractère

rot13("LBH QVQ VG!");
> "YOU DID IT!"
```

### Array.prototype.map

Cette fonction permet de transformer chaque élément d'un tableau avec un fonction `monTab.map(fct[,contexte])`

```javascript
var passengers = [ ["Thomas", "Meeks"],
                   ["Gregg", "Pollack"],
                   ["Christine", "Wong"],
                   ["Dan", "McGaw"] ];
var neoPassengers = passengers.map(function (element) {
  
  return element.join(" ");
});

console.log(neoPassengers);
> ["Thomas Meeks", "Gregg Pollack", "Christine Wong", "Dan McGaw"]
```

### Copie par référence

Attention lorsqu'on passe un tableau à une nouvelle variable on passe en fait la référence à ce tableau pas une copie

```javascript
var test = ["caca","kiki","coco","keke"];
var test2 = test;
test2.shift();
> "caca"
test2
> ["kiki", "coco", "keke"]
test
> ["kiki", "coco", "keke"]	
```

### Mutateur splice() retire des élément d'un tableau

tab.splice(indiceDebut,nbrElementAsupprimer[,elt1[,elt2...]]) renvoie les éléments retirés

Enlève des éléments d'un tableau à l'indice donné en nombre donné et les remplaces par les éléments passés en argument.

```javascript

var tab = ["caca","kiki","coco","keke"]

var tib = tab.splice(1,2,"titi","tata","toto")
tib
> ["kiki","coco"] // valeur de retour
tab
> ["caca", "titi", "tata", "toto", "keke"]

var tab = ["un","deux","trois"]
tab.splice(0,2) // depuis l'indice 0 retire deux éléments
tab
> ["trois"]
```