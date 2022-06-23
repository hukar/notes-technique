# 03 Typescript

##créer un fichier tsconfig.json

Dans le terminal :

```shell
tsc --init
```

## compiler un fichier

```shell
tsc nomFichier # n'utilise pas tsconfig.json
tsc # utilise tsconfig.json
```

## exemple de tsconfig.json

```json
{
    "compilerOptions": {
        "module": "system",
        "noImplicitAny": true,
        "removeComments": true,
        "preserveConstEnums": true,
        "outFile": "../../built/local/tsc.js",
        "sourceMap": true
    },
    "include": [
        "src/**/*"
    ],
    "exclude": [
        "node_modules",
        "**/*.spec.ts"
    ]
}
```

clause pour ne pas produire de js si erreur :

```json
{
  "compilerOptions": {
    "noEmitOnError": true,
    "watch": false // s'execute à chaque changement
  }
}
```

## fonction lambda

```typescript
var m = n=>2*n

m(4) // 8
```

une autre syntaxe :

```typescript
(x) => {return x + 5;}
```

## Cast avec `<>` et  `as`

### Avec l'opérateur diamant 

```typescript
interface Coco {
    name: string;
    age: number;
}

const ob = <Coco>{ name: 'titi'};

function greet(o: Coco) {
    console.log(o.name);
}

greet(ob);
```

Avec `as`

```typescript
interface Coco {
    name: string;
    age: number;
}

const ob = { name: 'titi'} as Coco;

function greet(o: Coco) {
    console.log(o.name);
}

greet(ob);
```

Bien entendu on a une erreur avec :

```typescript
interface Coco {
    name: string;
    age: number;
}

const ob = { name: 'titi'}/* rien ici */;

function greet(o: Coco) {
    console.log(o.name);
}

greet(ob); // <- erreur : Argument of type '{ name: string; }' is not assignable to parameter of type 'Coco'.
```

