# 02 Set up de l'application : `Core`

##  Comprendre le métier

### Lister les demandes de fonctionnalités : requirements



### ajouter des wireframes



## Créer le solution `.net`

On crée une solution `GloboTicket.TicketManagement`

```bash
dotnet new sln -n GloboTicket.TicketManagement
```

Ajouter des dossiers à la solution

- `src`
  - `API`
  - `Core`
  - `Infrastructure`
  - `UI`

- `test`

```bash
mkdir -p src/{API,Core,Infrastructure,UI} 
mkdir test
```

`-p` pou `parent`



## Créer le `Domain`

Le `Domain` est au centre de l'architecture et est constitué de `POCO` (`Plain Old CLR Object`).

Dans `Core` on crée un projet de `classlib` : `GloboTicket.TicketManagement.Domain`

```bash
dotnet new classlib -o src/Core/GloboTicket.TicketManagement.Domain
```

Dans `Domain` on crée un dossier `Entities` et on y met les entités déterminées par le business :

- Event
- Category
- Order



### `AuditableEntity`

On va ajouter un dossier `Common` et y créer la classe `AuditableEntity`.

Cette classe va servir a *tracker* les objets créé, les autres `classes` hérite de cette classe.

```cs
public class AuditableEntity
{
    public string CreatedBy { get; set; }
    public DateTime CreatedDate { get; set; }
    public string LastModifiedBy { get; set; }
    public DateTime? LastModifiedDate { get; set; }
}
```

```cs
public class Event : AuditableEntity
{
    // ...
```



## Créer l' `Application`

L'`Application` fait aussi partie du `Core`.

Elle utilise

- contrats
- messages



### Messages

Les `messages` permettent de communiquer entre composants sans créer de couplage entre eux.

On utilise `MediatR`.



### Contrats

Il font partie de `Application Core`.

Les fonctionnalités sont décrites à travers des interfaces.



### Utiliser le `Repository Pattern`

C'est un intermédiaire entre le `Domain` et la couche de `mapping`.

Il est souvent utilisé avec `Unit Of Work`.

On peut se poser la question de l'utilité de ce `Pattern` avec `EF Core`, car ce dernier reprend déjà beaucoup de fonctionnalités du `Repository Pattern`.

De même `Unit Of Work` peut être réalisé avec le `EF Core DbContext`.

<img src="assets/Capture%20d%E2%80%99e%CC%81cran%202021-05-18%20a%CC%80%2017.10.55.png" alt="Capture d’écran 2021-05-18 à 17.10.55" style="zoom:50%;" />



## Création du projet `Application`

```bash
dotnet new classlib -o src/Core/GloboTicket.TicketManagement.Application
```

```bash
dotnet sln add **/*.csproj
```

On y crée les dossiers :

-  `Contracts`
  - `Persisitence`



### Contrat générique

C'est dans ce dossier qu'on va créer le contrat (`interface`) :  `IAsyncRepository`

C'est un `repository` `generic` qui défini ce qu'on peut faire :

```cs
public interface IAsyncRepository<T> where T : class
{
    Task<T> GetByIdAsync(Guid id);
    Task<IReadOnlyList<T>> ListAllAsync();
    Task<T> AddAsync(T entity);
    Task UpdateAsync(T entity);
    Task DeleteAsync(T entity);
}
```

`where T : class` contrainte stipulant que `T` doit être une classe.

`IReadOnlyList<T>` une liste en `readonly` seulement.

### Contrats spécifiques

On va créer les interfaces spécifiques pour nos trois entités du `Domain` :

- `IEventRepository`
- `IOrderRepository`
- `ICategoryrepository`



On doit ajouter une référence du `Domain` dans `Application`.

```bash
dotnet reference Application add Domain
```

```bash
# syntaxe non simplifiée
dotnet add src/Core/GloboTicket.TicketManagement.Application reference src/Core/GloboTicket.TicketManagement.Domain 
```

Chaque interface spécifique ressemble à ça :

```cs
public interface IEventRepository : IAsyncRepository<Event>
{

}
```





















