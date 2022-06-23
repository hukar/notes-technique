# Animation d'un texte en js

L'idée est d'animé les point de suspension lors d'une attente de chargement.



```js
const p = document.getElementById("demo");

console.log;

const t = 700;
const loadTime = 8000;

const spelling = p => {
    console.log("spelling");
    let arrayText = p.innerText.split("");  // ici arrayText sera modifié donc let
    const saveArrayText = [...arrayText];

    p.innerText = "";

  	// la boucle d'animation, quand on a vidé le tableau de lettre, on le re-rempli avec sa version sauvée
    const animtext = setInterval(() => {
        if (arrayText.length == 0) {
            console.log("if");
            arrayText = [...saveArrayText]; // modification d'arrayText nécessitant let
            p.innerText = "";
        } else {
            const letter = arrayText.shift(); // shift le premier element du tableau pop le dernier
            p.innerText += letter;
        }
    }, t);
		
  // simule le temps de téléchargement après on clear l'animation
    setTimeout(() => {
        clearInterval(animtext);
    }, loadTime);
};

spelling(p);
```

## Le mot clé `const` et un `Array`

```js
const t = [];

t.push("titi");
t.pop();
t.pop();
t; //? []
 t = [] // Assignment to constant variable. 
```

`=` est l'opérateur d'assignement. Avec le mot clé `const` on peut modifier le contenu d'un tableau mais pas le réassigner.

