# Regex utiles

Comment retrouvé la première lettre de chaque mot

```javascript
var str = "The quick brown fox jumped over the lazy dog"
str.match(/\b\w/g)

// retourne ça
> ["T", "q", "b", "f", "j", "o", "t", "l", "d"]
```

caractère | signification
---|---
 \b | correspond à une limite de mot __o__ bliqu __e__
\B | correspond à une non-limite de mot o __bliqu__ e
Maintenant pour chaque première lettre en majuscule je crée une fonction __toUp__

```javascript
var toUp = function (match) {
 	return match.toUpperCase(); 
 };
 str.replace(/(\b\w)/g,toUp)
 
 // cela retourne :
 > "The Quick Brown Fox Jumped Over The Lazy Dog"
```

Une version améliorée qui accepte les guillemets (mais pas les caractères accentués)

```javascript
 str = str.replace(/(^|\s)(\w)/g,function (match) {
 	return match.toUpperCase();
 });
```

### Avec des accents et des caractères spéciaux

Voici la phrase :
`"À la Clair Éternel bij-pol Âda âtre ïop _poi /po ?kju !ui"`

```javascript
str.replace(/(^|\s)(?:[_/?!]*)+(\w|[ÀÉÂï])/g,function () {
 return (arguments[1] + arguments[2].toUpperCase());
 })
 
 // on obtient :
 > "À La Clair Éternel Bij-pol Âda âtre Ïop Poi Po Kju Ui"
```
* (^|\s)  recherche un début de chaîne ou un espace et ...
* (?:  motif non capturant
* [_/?!]* liste des caractère spéciaux possible a rechercher en zero ou plus occurence(s)
* )+ tout ça au moins une fois
* (\w|[ÀÉÂï])  capturer un caractère de mot ou liste des caractères accentués à capturer
* arguments 1 et 2 correspondent à $1 et $2 les parenthèses capturantes


# introduire une variable dans une regex

utilisation de `new RegExp`

Il faut juste échappé les caractères spéciaux d'expression régulière avec \

```js
// fonction permettant de retirer une classe
var removeClass = function (classStr,elt) {
  if (new RegExp(classStr).test(elt.className)) {
    elt.className = elt.className.replace(new RegExp('\\s?'+classStr),"")
  }
}

// on concatène '\\s' avec la variable
// \\s on échappe les caractères spéciaux
// pas de / ... / avant et après, on utilise des guillemets
```