Modularité

Le plus simple :

```typescript
import { IObserver } from "./IObserver";
import { ISubject } from "./ISubject";

export class Kiosque implements ISubject {
    ...
}
```

## Namespace

isole le code dans un block :

```typescript
namespace MyMath {
    const PI = 3.14;  // locale au namespace

    export function calculateCircumference(diameter: number): number {
        return diameter * PI;
    }  // export permet d'utiliser la fonction en dehors du namespace

    function calculateRectangle(width: number, length: number): number {
        return width * length;
    }
}

const PI = 99; // okay

console.log(MyMath.calculateRectangle(10, 20));  // erreur !!
console.log(MyMath.calculateCircumference(3));  // okay car export
```

On "sort" une variable avec le mot clé `export`

Pour compiler plusieurs fichiers en un seul :

```sh
tsc --outfile app.js file01.ts file02.ts app.ts
```

####! attention à l'ordre des fichiers .ts 

## import automatisé des namespaces



On utilise `/// <refrence />`  :

```typescript
/// <reference path="./funcA.ts" />
/// <reference path="./funcB.ts" />

funcTest.funcA()
funcTest.funcB()
```

Dans les fichiers on a :

```typescript
// funcA.ts
namespace funcTest {
    export const funcA = () => {
        console.log('I\'am func A');
    }
}

//funcB.ts
namespace funcTest {
    export const funcB = () => {
        console.log('I\'am func B');
    }
}
```

On compile avec :

```shell
tsc app.ts --outfile o.js
```

## Namespace avancé

namespace imbriqués

```typescript
//funcB.ts
namespace funcTest {

    export namespace nameB {
        export const funcB = () => {
            console.log('I\'am func B');
        }
    }   
}

//app.ts
/// <reference path="./funcA.ts" />
/// <reference path="./funcB.ts" />

funcTest.funcA()
funcTest.nameB.funcB()
```

Pour simplifier :

```typescript
/// <reference path="./funcA.ts" />
/// <reference path="./funcB.ts" />

import B = funcTest.nameB;
funcTest.funcA()
B.funcB()
```

# Modules

On utilise un module loader : `systemJs`

```sh
npm install systemjs --save
```

On ajoute dans le `index.html` :

```html
<script src="node_modules/systemjs/dist/system.js"></script>
    <script>
        SystemJS.config({
            baseURL:'/',
            packages: {
                '/': {
                    defaultExtension: 'js'
                }
            }
        });
        SystemJS.import('/modules/app.js');
    </script>
```

On utilise la commande :

```sh
tsc
```

et celà fonctionne avec la syntaxe `import` `export` :

```typescript
// dans app.ts
import { circum, PI } from "./math/circle";
import { rectangleCalculate } from "./math/rectangle";

// dans les fichiers importés :
export const PI = 3.14;

export function circum(rayon: number): number {
    ...
}
```



## syntaxe alternative

```typescript
// dans app.ts
import * as CC from "./math/circle";
import calc from "./math/rectangle";

// dans le duxième fichier importé
export default function rectangleCalculate(width: number, length: number): number {
    ...
}
```

`as`  pour créer un alias

`*`  pour tout importer

`default`  pour donner directement un nom dans l'import

