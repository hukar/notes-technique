# 02 Les `services`

## Ajouter un service

`Program.cs`

```cs
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddSingleton<CustomerRepository>();
```

Exemple avec `Swagger` :

```cs
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new() { Title = "oldSchholStymleAPI", Version = "v1" });
});
```



## Utiliser un `service` 

### Dans un `Delegate` de `Endpoint`

Les `services` sont directement liés (`Bindé`) avec le `delegate` :

```cs
builder.Services.AddScoped<IMyService, MyService>();

app.MapGet("/{id}", (int id, IMyService myService) => { ... });

class MyService { ... }
```

#### ! On déclare le type abstrait du `service` (l'`Interface`) comme paramètre du `Delegate` : `IMyService` et pas `MyService`.

On peut aussi le spécifier avec une `annotation` : `[Fromservices]`

```cs
app.MapGet("/customers/{id}", ([FromServices] ICustomerRepository repo, Guid id) => {
  // ...
```

Mais cela n'est pas nécessaire.

## Accéder à un service via le conteneur de services

```cs
var serviceProvider = builder.Services.BuildServiceProvider();
var mediator = serviceProvider.GetService<IMediator>();
```

### `serviceProvider`

Pour construire un service Provider :

```cs
var serviceProvider = builder.Services.BuildServiceProvider();
```



### `serviceProvider.GetService<MyService>`

Ensuite pour récupérer un service, c'est très simple :

```cs
var mediator = serviceProvider.GetService<IMediator>();
```



### Alternative directement depuis `app`

```cs
WebApplication app = builder.Build();

var mediator = app.Services.GetService<IMediator>();
```



