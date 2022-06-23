# 14 `API` `A`pplication `P`rogramming `I`nterface

Un lot de **classes**, **méthodes**, **fonctions** et de **constantes** accompagnés d'une description, par lequel un logiciel offre des services à un autre logiciel.

Une **signature de fonction** ou une **url** peuvent être considérés comme des `API`.

Pour le web, on parle d'`API` lorsqu'on manipule (envoyer, recevoir) des données via `HTTP` et `TCP/IP` grâce AU `URL` (`U`niform `R`essource `L`ocator).

Cette `URL` est pour l'`API` un `endpoint`.

#### `endpoint` : Une `URL` particulière faisant partie d'une `API`.

## `serialize`

Serialiser un objet, c'est le traduire dans un format où il peut être sauvegardé ou transféré

-> `JSON`, `CSV`, `XML` .

`CVS` : `C`omma `S`eparated `V`alues.

`deserialize` : convertir un de ces formats en objet.

```js
const server = http.createServer((req, res) => {
    res.writeHead(200, { "Content-type": "application/json" });

    const o = {
        name: "John",
        age: 44,
        pet: "Doggy"
    };

    res.end(JSON.stringify(o));
});
```

