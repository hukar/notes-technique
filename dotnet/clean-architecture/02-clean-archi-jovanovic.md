# 02 `Clean` Architecture - Milan Jovanovic



## Constituants par couche

### `Domain`

Ne peut pas référencer d'autres couches

- `Domain Entities`
- `Custom Exception`
- `Aggregates`
- `Value Objects`
- `Domain Events`
- `Repository Interface`
- `Services Interface`
- `Factory`



### `Application`

C'est l'orchestrateur du système.

- `Use Cases`
- avec `CQRS` on a les `Commands` et `Queries`



### `Infrastructure`

Tout ce qui utilise des systèmes externes.

- `Database Access`
- `Message Queues` => `RabbitMQ`, `Kafka`
- `Email`
- `Notification Services`
- `Storage services`



### `Presentation`

C'est un point d'entrée pour qu'utilisateur puisse interagir avec le système.

- `Rest API` ou `Grpc`



## La partie `Domain`

### `Entity`

On peut avoir une classe abstraite représentant une `Entity`:

```cs
public abstract class Entity
{
    public int Id { get; init; }
}
```



### `Custom Exception`

On peut avoir des `Custom Exceptions`:

```cs
public sealed class RobotNotFoundException : NotFoundException
{
    public RobotNotFoundException(int id)
        : base($"Robot with id {id} was not found") { }
}
```



### `IRepository` et `IUnitOfWork`

Les implémentations se trouvant ns la couche `Infrastructure`.

> Personnellement je mettrai les interafces dans le projet (la couche) `Application`.



## La partie `Application`

### Regroupement des `Use cases`

On peut les regrouper par `Entity` appelée (`root aggregate`) et grâce au pattern `CQRS`.

`MyEntity`

-  `Commands`
  - `CreateMyEntity`
    - **CreateMyEntityCommand.cs**
    - **CreateMyEntityHandler.cs**
    - **CreateMyEntityValidator.cs**
- `Queries`
  - `GetMyEntityById`
    - **GetMyEntityByIdQuery.cs**
    - **GetMyEntityByIdHandler.cs**



### `behaviours`

`MediatR` possède aussi un `pipeline` nous permettant d'ajouter des traitements avant (et après ?) les handler.

On peut y placer la `Validation`.

> Je préfère utiliser les `EndpointFilter` pour la validation, qui s'effectue avant même d'arriver dans le `RequestDelegate`.
>
> Par contre les `Validators` sont placés dans la couche `Application`, dans chaque `Use Case` ou dans un dossier `Validators`.
>
> Pour les exceptions la place idéal est dans un `Custom Middleware`. 



## La partie `Infrastructure`

### Le `DbContext`

### Les implémentations de `repository`

### Les `migrations`

Pour `EF Core`

### Les `Entities Configurations`

Pour `EF Core`



On pourrait diviser cette couche en deux avec `Persistance` et `External` (ou `Infrastructure` si c'est des projets séparés).



## La partie `Presentation`

On y trouve les `Controllers` ou les `Endpoints`.

Cette couche utilise le pattern `mediator`/`CQRS` pour communiquer avec la couche `Application`.

Dans cet exemple cette couche est séparée de l'application `Web` (le point d'entrée).



## La partie `Web`

Est le point d'entrée du système.

### Middleware

- `ExceptionMiddleware`
  Permet de gérer de manière globale les `Exception`.