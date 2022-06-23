# Interfaces

Problème :

```typescript
function displayName(Boob) {
    console.log(Boob.name);
}

displayName({name: "boooo"});
displayName({label: "rocky"}); // pas d'erreur à la compilation
```

Le résultat :

```sh
boooo
undefined
```

Pour éviter ça on utilise l'interface :

```typescript
interface IBoob {
    name: string;
    age?: number;    // paramètre optionnel avec ?
}

function displayName(boo: IBoob): void {
    console.log(boo.name);
}

displayName({label: "rocky"}); // erreur à la compilation
displayName({name: "boooo", age: 25});
displayName({name: "Bû"});  // pas d'erreur car age est optionnel avec ?
```

On utilise aussi interface avec des fonctions



```typescript
interface IObserver {
    update(): void;
}

interface ISubject {
    attach(io: IObserver);
    detach(io: IObserver);
    notify();
}
```

Si il n'y a qu'une signature on écrit :

```typescript
type ITestable = (n: number, s: string) => boolean;
```

