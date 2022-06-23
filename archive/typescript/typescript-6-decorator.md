# Decorator

Un decorator ajoute une (des) fonctionnalité à une classe ou à une méthode.



Dans le fichier `decorator-mylog.t`s :

```typescript
export default function mylog(target: object, key: string, descriptor: TypedPropertyDescriptor<any>) {
    const originalMethod = descriptor.value;

    descriptor.value = function(...args: any[]) {
        console.log(`${key} method called with args : ${JSON.stringify(args)}`);

        const result = originalMethod.apply(this, args);

        console.log(`${key} return value : ${JSON.stringify(result)}`);
    };

    return descriptor;
}
```

target: la méthode ciblée

key: le nom de la méthode

descriptor: un objet implémentant l'interface suivante :

```typescript
interface TypedPropertyDescriptor<T> {
    enumerable?: boolean;
    configurable?: boolean;
    writable?: boolean;
    value?: T;
    get?: () => T;
    set?: (value: T) => void;
}
```

descriptor.value est la fonction reçu et la fonction de retour.

Dans la classe on uytilise le decorator :

```typescript
import mylog from "./decorator-mylog"; // import du decorator

export class Robot {
   ...

    @mylog  // utilisation du decorator @ + le nom de la fonction
    public addTechnicalPart(pawa: number, action: string): TechnicalPart {
        const tp: TechnicalPart = new TechnicalPart(pawa, action);
        this.technicalPart.push(tp);

        return tp;
    }

    @mylog  // icic aussi
    public increaseName(surname: string, link: string): string {
        this.name = surname + link + this.name;

        return this.name;
    }
}

```

