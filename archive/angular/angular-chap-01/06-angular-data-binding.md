# 06 Angular Data Binding

Le data binding est une communication entre le code Typescript et le template html

## Différents binding

one way binding : string interpolation   ts -> html

```jsx
{{post}}
```

Two way binding  ts <- & -> html

```jsx
[(ngModel)]="value"
```

event binding  ts <- html

```jsx
(click)="onMyFunc()"
```

<span style="color:deepPink">**!** bonne pratique : faire commencer les fonction réagissant à un événement par ```on``` </span>

Attribut binding : property binding  ts -> html

```jsx
[class]="classBeautify()"
```





## interpolation de texte

Dans le fichier `stuck.component.ts`

```typescript
import { Component } from "@angular/core";

@Component({
    selector: 'app-server', // nom de la balise html <app-server></app-server>
    templateUrl: './server.component.html'
})

export class ServerComponent{
    serverConnection: number = 10; // ici les variables injectée dans le template
    serverStatus: boolean = true;
}
```

Dans le template `stuck.component.htm`

```html
<h3>Application server<small class="text-muted"> {{'is here!'}}</small></h3>
<p>
    server connection : {{serverConnection}} <br>
    server status : {{serverStatus?'online':'offline'}}
</p>
```

Interpolation avec `{{texte}}` :

. soit un string `{{ 'hello' }}`

. soit une variable `{{ maVar }}`

. soit n'importe quelle expression qui retourne un string `{{ getName() + "  " + boolVar?'lol':'mdr' }}`

. soit un type transformable en string : number, Array<number>, ...

```shell
# dans la classe
myNumbers: Array<number> = [1,2,3,4]; 

# dans le template
<p>
    {{ myNumbers }}
</p>

# à l'affichage
[1,2,3,4] # conversion automatique en string
```

## Binding d'attribut

Dans la classe de mon composant `stuck.component.ts`

```typescript
import { Component, OnInit } from '@angular/core';

@Component({
  selector: 'app-servers',
  templateUrl: './servers.component.html',
  styleUrls: ['./servers.component.css']
})
export class ServersComponent {
  // ici le booléen qui sera binder  
  allowNewServer: boolean = true;

  constructor() {
      
      // dans le constructeur une fonction qui changera 
      // la valeur de la variable binder dans 4s
    setTimeout(
      () => { this.allowNewServer = false; },
      4000
    );
  }
}
```

Dans le html : `stuck.component.html`

```html
<button class="btn btn-outline-primary"
        [disabled]="!allowNewServer">
        Add Server
</button>
```

On peut binder tous les attributs du DOM:

```html
<p [innerText]="'hello kitty'"></p>
```

On peut aussi utiliser l'interpolation, mais attention au mélange :

```html
<button class="btn btn-outline-primary" 
        disabled="{{ allowNewServer?'':'disabled' }}">
        Add Server
</button>
<p [innerText]="'hello kitty'"></p>
<p class="{{beauty}}">
    Lorem ipsum dolor sit, amet consectetur adipisicing elit. Rem ea cum quae quisquam odit itaque!
</p>
<p [class]="beauty">
    Lorem ipsum dolor, sit amet consectetur adipisicing.
</p>
```

## Binder les événements

Avec les parenthèses et sans le *on* :

```html
<button class="btn btn-outline-primary" 
        disabled="{{ allowNewServer?'':'disabled' }}" 
        (click)="onCreateServer()">
        Add Server
</button>
```

onCreateServer étant dans la classe :

```typescript
export class ServersComponent {
		// ...
  serverCreationStatus: string = "No server was created !";
		// ...
  onCreateServer() {
    
    this.serverCreationStatus = "server created";
  }
}
```

On peut directement écrire du code dans le template :

```html
<button  [class]="colorWarning?'btn btn-outline-warning':'btn btn-outline-primary'" 
              [disabled]="allowAutor"
              (click)="onHelloAutor()"
              (mouseover)="onChangeColor()"
              (mouseout)="colorWarning=false">Hello Autor</button>
```

```(mouseout)="colorWarning=false"```  tant que la logique est simple ce n'est pas gênant.

## Binding à la main

Dans le html:

```html
<label>{{serverName}}</label>
<input 
    type="text"
    class="form-control"
    (input)="onUpdateServerName($event)">
```

Dans la classe 

```typescript
export class ServersComponent {
		// ...
  serverName: string = '';
		// ...

  onUpdateServerName(event: Event) {
      
    this.serverName = (<HTMLInputElement>event.target).value;
  }
}
```

`(<HTMLInputElement>event.target)`  étant un cast vers input element

## Two Way Binding

avec `[(ngModel)]`



dans `app.module.ts`

ajouter deux lignes

```typescript
		// ...
import { FormsModule } from '@angular/forms'; // <= ici
		// ...

@NgModule({
  declarations: [
   // ...
  ],
  imports: [
    BrowserModule,
    FormsModule  // <= ici
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

Dans le html :

```html
<input 
    type="text"
    class="form-control"
    [(ngModel)]="serverName">
    <br>
    <label>{{serverName}}</label>
```

#### ! ne pas utiliser avec dans un formulaire `<form>` 

