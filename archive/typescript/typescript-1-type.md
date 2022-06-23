##Types

- boolean
- number
- string
- any = tous les types

```typescript
let dede: any = 5;
dede = "coco";
```

- number[]
- string[]
- any[]

```typescript
let stuff: any[] = ["titi", true, 1245, toto];
```

- void  pour les fonctions qui ne retourne rien

```typescript
function lg(st: any): void {
console.log(st);
}
```

## Type any

```typescript
let various;  // la même chose que let various: any;

various = 45;
various = "fred";  // pas d'erreur ici
```

Cela ne provoque pas d'erreur car `let various;` équivaut à écrire `let various: any;`

## Tuples

```typescript
let adress: any[];
adress = ["sesamestreet", 25];
adress = [214, "moscowstereet", "bat", 2];  // pas d'erreur

// tuples
let addr: [string, number];

addr = ["acaciaavenue", 135];
addr = [456, "waterlooavenue"];  // error : Type '[number, string]' is not assignable to type '[string, number]'.
```

```sh
Type '[number, string]' is not assignable to type '[string, number]'.
```

Les tuples déterminent de manière strict les éléments du tableau attendus.



## Enums

```typescript
// enum
enum Color {
    Gray,
    Green,
    Blue,
}

let mycolor: Color = Color.Green;

console.log(mycolor); // affiche 1

let secondcolor = Color[0];

console.log(secondcolor);  // affiche Gray
```

Teste en attribuant une valeur à la main :

```typescript
enum Color {
    Gray,
    Green = 3,  // on définit celui-ci à 3
    Blue = 2,  // si on ne met rien = 4 (après 3).
    Yellow,
    Orange,
    Violet,
}
    
// cela compile en :
Color[Color["Gray"] = 0] = "Gray";
Color[Color["Green"] = 3] = "Green";
Color[Color["Blue"] = 2] = "Blue";
Color[Color["Yellow"] = 3] = "Yellow";   // <- cela écrase le précédent 3
Color[Color["Orange"] = 4] = "Orange";
Color[Color["Violet"] = 5] = "Violet";

affichage de Color :
    { '0': 'Gray',
  '2': 'Blue',
  '3': 'Yellow',
  '4': 'Orange',
  '5': 'Violet',
  Gray: 0,
  Green: 3,
  Blue: 2,
  Yellow: 3,
  Orange: 4,
  Violet: 5 }
```

Il manque une entrée chiffre.



## Type fonction

```typescript
function sayHello(): void {
    console.log("hello coco!");
}

function multiply(a: number, b: number): number {
    return a * b;
}

let myFunction: (val1: number, val2: number) => number;

myFunction = sayHello;  // error : '() => void' is not assignable to type 
					  // '(val1: number, val2: number) => number'

myFunction();  // error !

myFunction = multiply;  // Ok

console.log(myFunction(2, 3));
```

type de la form `() => type de retour`  pour exemple : `(val1: number, val2: number) => number`

## Type Object

```typescript
// object type

let coco: {specie: string, age: number};

coco = {
    specie: "cat",
    age: 5
};

coco = {a: "minou", b: 12};  // error :
						// Type '{ a: string; b: number; }' is not assignable to type 
						// '{ specie: string; age: number; }'
```

## Type alias

```typescript
// object type

let complex: {data: number[], output: (all: boolean) => number[]} = {
    data: [12, 13, 14],

    output: function(all: boolean): number[] {
        return this.data;
    }
};

// type alias

type TComplex = {data: number[], output: (all: boolean) => number[]};

let complex2: TComplex;  // <- beaucoup plus court et maintenable
complex2 = {
    data: [1, 6],

    output : function(all: boolean): number[] {
        return this.data;
    }
};
```

Voila la déclaration et l'assignation d'un type :

```typescript
type TComplex = {data: number[], output: (all: boolean) => number[]};
```

##Union Types



```typescript
// union types

let myRealAge: number | string | boolean;

myRealAge = 27;
myRealAge = "vieux";

myRealAge = true;

myRealAge = [1, 2, 3];
// Type 'number[]' is not assignable to type 'string | number | boolean'
```



`|` pipe est l'opérateur d'union de type => on veut ce type et ce type mais pas les autres (contrairement à any).

## type de retour never

Il n'y a jamais de retour :

```typescript
function err(): never {
    throw new Error("an error !");
}
```

##type NULL

```typescript
// null

let canBeNull: null | number = 12;
canBeNull = null;  // ok avec l'union types

let canotBeNull = 13;
canotBeNull = null; // error: Type 'null' is not assignable to type 'number'.
```

