# 07 Angular Directives

## *ngIf

`*ngIf` reçoit un booléen et **ajoute** ou **retire** du DOM l'élément cible.

```html
<p *ngIf="serverCreated">  <!--expression booléenne-->
    Server was created server name is : {{serverName}}
</p>
```

## clause else

```html
<p *ngIf="serverCreated; else noServer">
    Server was created server name is : {{serverName}}
</p>
<ng-template #noServer>
    <p>
        no server here!
    </p>
</ng-template>
```

## ngStyle

ajoute un attribut style dynamiquement

```html
<div [ngStyle]="{backgroundColor:getColor()}">
   ...
</div>
```

Attention aux crochet pour binder l'attribut.

Attention aussi à la syntaxe **camelCase** des propriétés css

On peut utliser une fonction pour générer la valeur.

```typescript
getColor() {
    return this.serverStatus === 'online' ? 'red' : 'green';
}
```

## [style] ne fonctionne pas

Cette forme provoque un warning :  ```WARNING: sanitizing unsafe style value```

Pour prévenir les failles XSS Angular ne permet pas certains caractères.

par contre ceci fonctionne :

```html
<div [style.color]="mycolor"> premier [style] </div>
```

avec dans le ```component.ts```:

```typescript
 mycolor = 'color:deepPink';
```

## ngClass

ajoute dynamiquement une classe

[ngClass]="{'nom-de-la-classe': (expression retournant true ou false),nomDeClasse:true,className:condition}"

```html
<div 
    [ngStyle]="{color:getColor()}"
    [ngClass]="{alert:true,'alert-danger': !(serverStatus==='online'), 'alert-success': isOnline()}">
    ...
</div>
```

## [class.ma-class] = bool

écriture alternative pour définir une classe avec un booléen :

```html
<button class="btn btn-secondary"
        (click)="flag=!flag">change my bro</button>
<button [ngClass]="{btn: true}" 
        [class.btn-outline-success]="flag"
        [class.btn-outline-info]="!flag">yo coco</button>
```



#* directive de structure == change le DOM

## *ngFor

```html
<app-server *ngFor="let server of servers"></app-server>
```

```typescript
servers = ['sever one','server two'];
```

### récupérer l'index de boucle

L'index comence à 0

```html
<p *ngFor="let logItem of log; let i = index" 
    [innerText]="logItem + ' ' + i"
    [ngStyle]="{backgroundColor: i >= 5 ? 'blue' : 'transparent'}"
    [ngClass]="{'white-text': i >= 5}">
</p>
```
## ng-content

permet de mettre du contenu dans le template parent du composant :

template du composant enfant :

```html
<div class="card mb-2">
    ...
    <div class="card-body">
        <ng-content></ng-content>
    </div>
</div>
```

template du composant parent :

```html
<app-server-element *ngFor="let server of servers" [element]="server">
    <p>
        <strong *ngIf="server.isBlueprint===false">{{server.content}}</strong>
        <em *ngIf="server.isBlueprint===true" class="text-primary">{{server.content}}</em>
    </p>
</app-server-element>
```

## [ngSwitch]

```html
<div [ngSwitch]="size" class="col">
    <p *ngSwitchCase="5">petit</p>
    <p *ngSwitchCase="10">moyen</p>
    <p *ngSwitchCase="15">grand</p>
    <p *ngSwitchDefault="">inconnue</p>
</div>
```

