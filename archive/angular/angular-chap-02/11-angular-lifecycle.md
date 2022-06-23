# lifecycle hooks

## ngOnInit vs constructor

```mermaid
graph LR
a(constructor)-->b(ngOnChanges)
b(ngOnChanges)-->c(ngOnInit)

```

Le constructeur s'exécute avant que le composant ne soit créé :

```typescript
export class ListingActiveComponent implements OnInit {

    @Input('test') name: string;
    text01: string;
    text02: string;

    constructor() {
        this.text01 = 'hello ' + this.name + ' test of constructor power';
        console.log(this.name);
    }

    ngOnInit(): void {
        this.text02 = 'hello ' + this.name + ' test of ngOnInit power';
        console.log(this.name);
    }
}
```

On obtient l'affichage :

```sh
name : coco
hello undefined test of constructor power
hello coco test of ngOnInit power
```

Comme name est une propriété binder sur le template, elle n'existe pas encore au moment où le constructeur s'exécute.

Si des données sont susceptibles d'être généré par le DOM, il vaut mieux alors les traiter dans `ngOnInit`.