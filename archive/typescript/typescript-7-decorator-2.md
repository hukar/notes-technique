#Decorator @

un decorator est juste une fonction :

```javascript
function logger(...){ ... }
```
Pour une classe, le decorator récupère la référence vers le constructeur de la classe :

```typescript
function logged(constructorFn: Function) {
    console.log(constructorFn);
}

@logged
class Person {
    constructor() {
        console.log("hello gogos !!");
    }
}	
```

##Utiliser une factory

```typescript
// factory

function logging(value: boolean) {
    return value ? logged : null;
}
```

Il faut configurer `tsconfig.json` pour ne pas avoir une erreur de type avec le retour



```sh
Type '((constructorFn: Function) => void) | null' has no compatible call signatures.
```

`tsconfig.json`

```json
"strictNullChecks": false,   // au lieu de true par default
```

utilisation si true le log est afficher si false rien ne s'affiche :

```typescript
@logging(false)
class car {

}
```

## exemple avancé

```typescript
function printable(constructFn: Function) {
    constructFn.prototype.print = function() {
        console.log(this);
    }
}

@printable
class Plant {
    name = "Green Plant";
}

const p = new Plant();

(<any>p).print();
```

`(<any>)` est un cast générique obligé car sinon p.print() provoque une erreur :

```sh
Property 'print' does not exist on type 'Plant'
```

On peut mettre plusieurs decorator :

```typescript
@logging(true)
@printable
class Plant {
    name = "Green Plant";
}
```



## decorator de méthode

exemple pour rendre une propriété non redéfinissable (modifier la valeur de writable) :

```typescript
// factory
function editable(flag: boolean) {
    return function(target: any,propName: string, descriptor: PropertyDescriptor) {
        descriptor.writable = flag;
    }
}
```

target : l'objet possédant la méthode

propName : le nom de la méthode

descriptor : un objet de configuration de la méthode =>

```json
{value: ƒ, writable: false, enumerable: true, configurable: true}
```

## decorator de propriété

```typescript
// property decorator
function overwritable(value: boolean) {
    return function(target: any, propName): any {
const newDescriptor: PropertyDescriptor = {
    writable: value
}; // obj descriptor

        return newDescriptor;
    }
}
```

Cette fois descriptor est renvoyer par la fonction decorator et n'est plus un paramètre de celle ci;

```typescript
@overwritable(false)
public name: string; // on ne peut plus assigner cette variable	
```

## Decorator de paramètre

```typescript
// parameter decorator

function printInfo(target: any, methodName: string, index: number) {
    console.log("target", target);
    console.log("methode name", methodName);
    console.log("index", index);   
}
```

Et dans la fonction :

```typescript
printStudentNumbers(mode: string, @printInfo printAll: boolean) {
       ...
}
```

