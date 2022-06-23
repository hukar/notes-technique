## Binder une propriété à l'extérieur de l'élément



attribut à binder : 

```typescript
// dans sa classe
import { Component, Input } from '@angular/core'; // on ajoute Input
	// ...
export class ServerElementComponent {

    @Input() element: Server;

}
```

utilistation de `@Input()`  attention aux parenthèse

dans le template :

```html
<app-server-element *ngFor="let server of servers" [element]="server"></app-server-element>
```

Sans le `@Input()` , `[element]`  ne serait pas *'visible'*  depuis le html



## Avec un alias (bonus)

```typescript
@Input('aliasName') element: Server;
```

dans le template html :

```html
<app-server-element *ngFor="let server of servers" [aliasName]="server"></app-server-element>
```

## Binding custom event

### Établir une communication entre un composant et le composant parent

dans le composant parent :

```html
<app-cockpit (serverCreated)="onServerAdded($event)"></app-cockpit>
```

deux événement custom bindés : `(serverCreated)``

On passe les données avec l'objet `$event`  définie dans la méthode du composant parent:

```typescript
public onServerAdded(serverData: {serverName: string, serverContent: string}) {
		console.log('yo! server');

		this.servers.push(new Server(serverData.serverName, `new : ${serverData.serverContent}`, this.count, false));
		this.count += 1;
	}
```

On voit que dans ce cas `$event` est un objet de type `{serverName: string, serverContent: string}`

#### maintenant on doit émettre un événement depuis le composant enfant

```typescript
import { Component, EventEmitter, Output } from '@angular/core'; // EventEmitter et Output
import { Server } from '../server.model';

@Component({
	selector: 'app-cockpit',
	templateUrl: './cockpit.component.html'
})
export class CockpitComponent {
	// decorator Output pour rendre serverCreated accessible à l'extérieur
	@Output() serverCreated = new EventEmitter<{serverName: string, serverContent: string}>();  // new EventEmitter<Type>(); type de $event
	
	public nameServer;
	public contentServer;

	public addServer() {
		this.serverCreated.emit({
			serverName: this.nameServer,
			serverContent: this.contentServer});
	}
}

```

`serverCreated` est une propriété de type événement (émetteur d'événement) et accessible à l'extérieur grâce à `@Output`

### **!**  `@Output`  doit recevoir un `EventEmitter`

On utilise cet objet avec `this.serverCreated.emit({...})` 

#### ! attention d'avoir le bon import

## Avec un alias

```typescript
@Output('sCreated') serverCreated = new EventEmitter<{serverName: string, serverContent: string}>();
```

dans le html :

```html
<app-cockpit (sCreated)="onServerAdded($event)"></app-cockpit>
```

