# Classes

```typescript
class Robot {

    public static enginemodel: string = "megatronic engine";
    private static powerengine: number = 500000;

    public denominator: string;
    private constructdate: Date;

    constructor(d: string, c: Date) {
        this.denominator = d;
        this.constructdate = c;
    }
}

const nono = new Robot("nono", new Date());

console.log(nono);
console.log(Robot.enginemodel);

console.log(Robot.powerengine);  // error is private
```

## overload method - surcharge de méthode

on précise les signatures acceptées et on implémente la méthode :

```typescript
public notify(args: undefined): void;

public notify(args: object): void {
    args = args ? args : {};
    if (this.changed === true) {
        for (const observer of this.listObserver) {
            observer.update(this, args);
        }
    }
    this.changed = false;
}
```

Dans ce cas on aurait pu écrire :

```typescript
public notify(args?: object): void {
        args = args ? args : {};
        if (this.changed === true) {
            for (const observer of this.listObserver) {
                observer.update(this, args);
            }
        }
        this.changed = false;
    }
```

On utilise `args?` pour préciser que le paramètre est facultatif.

## Cast

```typescript
 public update(o: Observable, a: object): void {

        let k: Kiosque;
        k = o as Kiosque;  // o caster en Kiosque
        this.infos = k.getInfos();
        this.displayInfos();
    }
```

## Ecriture raccourci de classe

```typescript
export class Ingredient {
    public name: string;
    public amount: number;

    constructor(name: string, amount: number) {
        this.name = name;
        this.amount = amount;
    }
}
```

peut se raccourcir en :

```typescript
export class Ingredient {

    constructor(public name: string, public amount: number) {}
}
```

## Class Abstraite

```typescript
export abstract class Car {  // <- ici classe abstraite
    speed: number;

    color: string;

    abstract run(): void;  // <- ici méthode abstraite

    getMyPimp(): void {
        
       console.log('my beautiful color :' + this.color);
    }
}
```

utilisation de `abstract class` et `abstract myMethod`