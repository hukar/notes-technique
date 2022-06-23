# AA - Bonnes Pratiques : Clean Architecture

## Speach de Jason Taylor

https://www.youtube.com/watch?v=dK4Yb6-LxAk&t=2055s

- `Domain` contient la logique et les types de l'entreprise.

- `Application` contient les types et la logique de l'application.
- `Infrastructure` contient toutes les implications extérieur.
- `Presentation` et `Infrastructure` dépendent uniquement de la couche `Application`.
- Les composant `Infrastructure` et `Presentation` peuvent être remplacés avec peu d'efforts.



Le `Core` contient des abstractions (`Interface`) qui seront implémenter par les couches `Infrastructure` et `Presentation`, par exemple `Appliaction` contient un `Irepository` implémenté par la couche `Infrastructure`.

## Core

- Indépendant des `Frameworks`
- Testable
- Indépendant des `UI`
- Indépendants des `DB`

## Domain

Il contient :

-  `entities`
- `Value Object` : un type qui a un sens de valeur plutôt que de référence. (des couleurs, des degré (Celsius, Farheneit), des devises, ... ). Ne pas utiliser les `record` mais plutôt hériter de la classe proposée par Microsoft : https://docs.microsoft.com/en-us/dotnet/architecture/microservices/microservice-ddd-cqrs-patterns/implement-value-objects
- `Enum`
- `Logic`
- `Custom Exception`



### Points à respecter

> ### Initialiser toutes les collections et mettre `private set;`

- Éviter d'utiliser les `Data Annotations` préférer `Fluent Validation`
- Utiliser les `Value Object` plutôt que les type primitif si de la logique est associée à la donnée
- Créer des `Custom Domain Exception` pour savoir où se trouve le problème
- **Initialiser toutes les collections** et utiliser `Private setters`
- *Tracker* automatiquement les entitées en les faisant hériter d'une `Base Class` : `AuditableEntity`



#### Il faut que les mauvaises choses soient dures à faire et que les bonnes soient faciles



# Application

- `Interfaces` (implémentés dans la couche `Infrastructure`)
- `Models`  : `View Models` et `DTO`

- `Logic`
- `Commands` / `Queries`
- `Validators`
- `Custom Exceptions`



## `CQRS` Command Query Responsability Segregation

Séparer les actions de lecture et d'écriture :

- Maximise les performances, la scalabilité et la simplicité
- Facile d'ajouter une nouvelle fonctionnalité, il suffut d'ajouter uen `Query` ou une `Command`
- Facile à maintenir, un changement n'affectement seulement qu'une `Query` ou une `Command`



## `CQRS` + `MediatR` = ❤️

Définie les `Commands` et les `Queries` comme des requêtes.

La couche `Application` est juste une série d'objet de requête et de réponse.

On a la possibilité d'ajouter des comportement avant et/ou après chaque requête, par exemple : `Validation`, `Logging`, `Caching`, `Authorization` et autre.



## Utilisation imbriquée de `IRequest` et `IRequestHandler`

```cs
public class GetTodosQuery : IRequest
{
    // ...
    
	public class GetTodosQueryHandler : IRequestHandler
    {
        // ...
    }
}
```

Plus facile à lire.

Si on utilise les `record` pour les `Request`, il vaut peut-être mieux ne pas les imbriquer :

```cs
public record GetTodosBySizeQuery(int size) : IRequest;

public class GetTodosBySizeHandler : IRequesthandler
{
    // ...
```



### Organisation des dossiers

<img src="assets/organisation-folder-application.png" alt="organisation-folder-application" style="zoom:50%;" />

Dans le projet `Application`, on a un dossier `Common` pour tous les traitements transverse et les différentes fonctionnalités (`Features`).

Chaque `feature` est organisé en `Queries` (lectures) et `Commands` (écritures) contenant tous les deux des `Request`.

Ainsi organisé, chaque dossier de `Request` contient la `Request` et tous ce qui lui est nécéssaire : `DTO`, `VM`, `Request` et autre.

La logique métier est entiérement contenu dans la requête.

La `Query` est aussi une `DTO`.

Pour les `Request` et les `DTO` il peut être intéressant d'utiliser des `record`.



## Dossier `Common`

<img src="assets/cross-cutting-concerns.png" alt="cross-cutting-concerns" style="zoom:50%;" />

Centralise les traitements (réalisés par la couche `infrastructure`) pour toutes les requêtes.

Utilise le `pipeline` de `MediatR` et `IRequestPreProcessor`.

#### Avant la `Request` : `IRequestPreProcessor`

#### Pendant la `Request` : `IPipelineBehavior`

### `AutoMapper`

Implémentation centralisé et automatisé (à voire).

### `DependencyInjection`

```cs
public static class DependencyInjection
{
    public static IServiceCollection AddApplication(this IServiceCollection services)
    {
        services.AddAutoMapper(Assembly.GetExecutingAssembly());
        services.AddValidatorsFromAssembly(Assembly.GetExecutingAssembly());
        services.AddMediatR(Assembly.GetExecutingAssembly());
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(UnhandledExceptionBehaviour<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(AuthorizationBehaviour<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(ValidationBehaviour<,>));
        services.AddTransient(typeof(IPipelineBehavior<,>), typeof(PerformanceBehaviour<,>));

        return services;
    }
}
```

`AddTransient` un nouvel objet chaque fois que le service est demandé.

Créer une méthode d'extension pour ajouter facilement les dépendances dans `Startup.cs` :

```cs
public void ConfigureServices(IServicesCollection services)
{
    services.AddApplication();
}
```



## `IDateTime`

Utilisation d'une `interface` pour les dates.

Sépare l'horloge système de la couche application, utile apparemment pour les tests (?).

## Points clés

- Utilisation de `CQRS` + `MediatR` pour simplifier le design général
- `Fluent Validation` est utile pour les validations simples et complexes
- `AutoMapper` simplifie le *mapping* et les projections
- `Application` est indépendant de `Infrastructure`



# `Infrastructure`

Cette couche contient :

- `Persistence` (`EF Core`)
- `Identity`
- `File System`
- `System Clock` (`DateTime`)
- `API Client`

On peut séparer les différentes `infrastructures` avec des `projets` ou des `dossiers`.

## Faut-il implémenter `Unit Of Work` et `Repository Pattern` ?

`EF Core` a déjà des alternative (implémentation ?) de ces patterns :

- `DbContext` fonctionne comme `Unit Of Work`
- `DbSet` fonctionne comme un `Repository`

Si on veut une abstraction sur l'`ORM` (Object relationnal Mapping) et non sur la `BDD`, alors on utilise un `repository`.

### Avis d'expert

Jimmy Bogard (`AutoMapper`, `MediatR`) :

> J'en ai fini avec `Repository`, et définitivement fini avec abstraire votre couche de données.

 Steve Smith (Microsoft MVP)

> Non vous n'avez pas besoin d'un `Repository`, mais il y a certain bénéfices que vous devriez considérer.

John Smith (auteur de `EF Core in action`)

> Non, Les patterns `Repository/Unit-Of-Work` ne sont pas utiles avec `EF Core`.



## Organisation

### 4 dossier

- `Files`
- `Identity`
- `Persistence`
- `Services`



### 1 Fichier

`DependancyInjection.cs` 



## Persistence

### `EF Core`

Ne pas utiliser de configurations redondantes avec les conventions.

`EF Core` automatise déjà beaucoup de comportement grâce aux conventions.

Il faut travailler avec les **conventions**.

C'est dans `ApplicationDbContext` que `AuditableEntity` est renseigné :

```cs
foreach (Microsoft.EntityFrameworkCore.ChangeTracking.EntityEntry<AuditableEntity> entry in ChangeTracker.Entries<AuditableEntity>())
{
    switch (entry.State)
    {
        case EntityState.Added:
            entry.Entity.CreatedBy = _currentUserService.UserId;
            entry.Entity.Created = _dateTime.Now;
            break;

        case EntityState.Modified:
            entry.Entity.LastModifiedBy = _currentUserService.UserId;
            entry.Entity.LastModified = _dateTime.Now;
            break;
    }
}
```



### Points Clés

- Indépendant d'une `DB` particulière (il faut juste reconstruire les `migrations`)
- Dépendant d'`EF Core`
- Utiliser `Fluent API Configuration` plutôt que les `Data annotations`
- Préféré les conventions plutôt que les configurations
- Aucune couche ne dépend d'`Infrastructure` à part la couche `Presentation`



# `Presentation`

## `Application API`

Le contrôleur n'a plus de logique, il n'utilise pas ditectement `DbContext` :

```cs
[HttpGet("{id}")]
public async Task<FileResult> Get(int id)
{
    var vm = await Mediator.Send(new ExportTodosQuery { ListId = id });

    return File(vm.Content, vm.ContentType, vm.FileName);
}

[HttpPost]
public async Task<ActionResult<int>> Create(CreateTodoListCommand command)
{
    return await Mediator.Send(command);
}
```

Le contrôleur se contente d'envoyer des messages via `Mediator`.

Il est le plus simple possible et ne contient pas de logique.

Il hérite de `ApiControllerBase` :

```cs
namespace CleanArchitecture.WebUI.Controllers
{
    [ApiController]
    [Route("api/[controller]")]
    public abstract class ApiControllerBase : ControllerBase
    {
        private ISender _mediator;

        protected ISender Mediator => _mediator ??= HttpContext.RequestServices.GetService<ISender>();
    }
}
```

si `_mediator` est `null` alors `_mediator = httpContext.RequestServices.GetService<ISender>()`

`??=`  : si la partie de droite est `null` alors assigne la partie de gauche.

### `Custom Exception`

Utilise un `midleware` pour intercepter les `exceptions`.

### `Identity`

## Points Clés

- Les contrôleur ne doivent pas contenir de logique d'application
- Créer et utiliser des `View Models` bien définis
- Swagger Open Api

## Tests

`Unit Test`

`Integration Test`

