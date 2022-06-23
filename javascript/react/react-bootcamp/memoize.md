# memoize : 

# un système de mise en cache

```js
// memoization

const memoize = function(f) {
    const cache = {};

    return function() {
        const arg_str = JSON.stringify(arguments);
        cache[arg_str] = cache[arg_str] || f.apply(null, arguments);
        return cache[arg_str];
    };
};

const compute = memoize((a, b) => {
    const result = a * 2 + b * 3;
    console.log("compute work");
    return result;
});

compute(5,6); //? 28 compute work
compute(5,6); //? 28  
compute(5,3); //? 19 compute work
compute(5,3); //? 19
compute(6,3); //? 21 compute work
compute(5,6); //? 28
```

`memoize ` prend une fonction en argument, créer un cache, vérifie si le résultat est dans le cache ou exécute la fonction, puis renvoie le résultat.

On index avec les arguments transformés en chaîne de caractères.

Si les mêmes arguments ré-apparaissent, inutile de re-exécuter la fonction, il suffit de renvoyer la valeur stockée dans l'objet-tableau_associatif.

