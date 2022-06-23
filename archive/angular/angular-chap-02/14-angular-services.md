# Services et Injection de dépendances

Les services vont centraliser des fonctionnalités ou des données pour être le plus **DRY**

**Don't Repeat Yourself**

## Création d'un service

#### rappel 

`./` dossier actuel

`../` dossier parent

`/` dossier racine

----

##Un service simple

D'abord créons un fichier `nomDuService.service.ts` :

```typescript
export class LoggingService {

    logAccountsChange(account: string) {
        console.log('A sever status changed, new accounts: ', account);
    }
}
```

####! Un service est une juste classe.

Dans le composant :

```typescript
import { LoggingService } from '../services/logging.service'; // <- 1. import

@Component({ ... })
export class NewAccountComponent {
            
  myLoggingService = new LoggingService();  // 2. instanciation

  onCreateAccount(accountStatus: string) {

    this.myLoggingService.logStatusChange(accountStatus);  // 3. utilisation
  }
}
```



## Injection de Dépendance

Angular offre un mécanisme hiérarchique d'injection de dépendance

Angular injecte automatiquement une instance du service demandé, **il est responsable de l'instanciation**

Maintenant voyons dans le composant utilisateur :

```typescript
	...
import { LoggingService } from './shared/logging.service'; // <- 1. ici

@Component({
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.css'],
  providers: [LoggingService]  // <- 2. ici on injecte la dépendance (instanciation)
})
export class AppComponent {
  ...
  constructor(private loggingService: LoggingService) {} // <- 3. on le déclare dans le constructeur

  updateAccounts(account: Account) {
    this.accounts.push(account);
    this.loggingService.logAccountsChange(this.accounts);  // <- 4. utilisation
  }

  logState() {
    this.loggingService.logAccountsChange(this.accounts);  // <- 4. utilisation
  }

}
```



1. on importe le service

2. on injecte sa dépendance via le provider

3. on le déclare dans le constructeur

4. on l'utilise

## Gérer ses DATA avec un service

un exemple de service simple `monServiceData.service.ts` :

```typescript
export class MonServiceDataService {
    data = [
        { ... },
        { ... }
    ];
    
    addData(attr1: number, attr2: string) {
        this.data.push({attr1: attr1, attr2: attr2});
    }
    
    updateData(id: number, newValue: string) {
        this.data[id].attr2 = newValue;
    }
}
```

Dans un composant `myComponent.component.ts` :

```typescript
import { MonServiceData } from './services/monServiceData.service' // 1. import

@Component({
    ...,
    providers: [MonServiceData]  // 2. injection
})
export class MyComponentComponent implements OnInit {
    myDatas: {attr1: string, attr2: string}[] = [];  // 3. receptacle
    
    constructor(private monServiceData: MonServiceData) {}  // 4. declaration
    
    onInit() {
        this.myDatas = this.monServiceData.data;  // 5. utilisation
    }
}
```

## Hiérachie de l'injection de service Hierarchical injector

Le plus haut niveau où on peut déclarer un service est dans `app.module.ts` :

```typescript
// ...
import { AccountsService } from './shared/accounts.service';
import { LoggingService } from './shared/logging.service';

@NgModule({
  // ...
  providers: [LoggingService, AccountsService],  // <- ici
  // ...
})
export class AppModule { }
```

Un service déclaré dans un composant est utilisable par ses enfants et c'est la même instance.

C'est toujours des parents vers les enfants et pas l'inverse.



#### ! il y a une instance du service par composant injectant le service

Un nouvel appel annule un ancien appel.



## Injecter un service dans un autre : `@Injectable`

#####Pour pouvoir être injecter les uns dans les autres, les services doivent être déclarés dans `app.module.ts`

Pour qu'un service puisse en utiliser un autre il faut lui ajouter le decorator `@injectable()`

Il récupérera ainsi les **metadata** nécéssaire à Angular pour l'injection de service.

`@Component` et `@Directive` apporte déjà les **metadata** attendus.

Exemple où `AccountService` veut utiliser `LoggingService` :

```typescript
// on est dans AccountService
import { Account } from './account.model';
import { LoggingService } from './logging.service'; // <- 1. on importe le service
import { Injectable } from '@angular/core';  // <- 2. on importe le decorator @Injectable

@Injectable()  // <- ici
export class AccountsService {
    accounts: Account[] = [
        new Account('Master Account', 'active'),
        new Account('Test Account', 'inactive')
    ];

    constructor(private loggingService: LoggingService) {}  // <- 3. on injecte le service dans le constructeur

    addAccount(account: Account) {
        this.accounts.push(account);
        this.loggingService.logAccountsChange(this.accounts);  // <- 4. on utilise le service
    }

    updateStatus(id: number, status: string) {
        this.accounts[id].state = status;
        this.loggingService.logAccountsChange(this.accounts);  // <- 4. on utilise le service
    }
}

```

1. On importe le service
2. On importe le decorator @Injectable
3. On injecte le service dans le constructeur
4. On utilise le service dans les méthodes du service utilisateur

## Communication entre les composants grâce aux service

On peut émettre  un évènement depuis un composant et l'écouté dans un autre composant en passant par un service :

Service :

```typescript
...

@Injectable()
export class AccountsService {
    ...
    accountChanged = new EventEmitter<string>();  // <- 1. on crée un évènement
}

```

Dans le composant A :

```typescript
@Component({ ... })
export class AccountComponent {
	...
  constructor(private accountService: AccountService) {}  // <- on récupère le service

  onSetTo(status: string) {
    this.accountService.updateAccount(this.id, status);
    this.accountService.statusChanged.emit(status);  // on emet un événement
  }
}
```

Dans le composant B :

```typescript
@Component({ ... })
export class NewAccountComponent {

  constructor(private accountService: AccountService) {

    this.accountService.statusChanged.subscribe(   // <- on s'abonne
           
      (status: string) => alert('hello ' + status);
    );
  }
}
```

`subscribe` prend une fonction qui sera exécutée à chaque fois que l'événement statusChanged sera émit.

## @Injectable au lieu de provider[]

On peut écrire ça :

```typescript
@Injectable({providedIn: 'root'})
export class MyService { ... }
```

Au lieu de ça :

```typescript
export class MyService { ... }
```

plus ça: 

```typescript
import { MyService } from './path/to/my.service';
 
@NgModule({
    ...
    providers: [MyService]
})
export class MyService { ... }
```

