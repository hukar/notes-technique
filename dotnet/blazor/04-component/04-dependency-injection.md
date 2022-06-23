# 04 Injection de dépendance dans `Blazor`

## Service déjà présent

Par défaut trois service sont déjà présent dans une application `Blazor`.

1. `HttpClient` pour exécuter des requêtes `HTTP` (`scoped`)
2. `IJSRuntime` pour utiliser `javascript`. (`singleton`)
3. `NavigationManager`permet d'utiliser la navigation dans le code du composant.(`singleton`)

`Program.cs`

```cs
builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
```

> Pour les deux autres je ne sais pas ??



## Durée de vie des services

- `scoped` vie le temps d'un `context`(pendant une requête `HTTP`)

  > Les applications `Blazor WebAssembly` n'ont pas actuellement de concept de scopes DI. Les services enregistrés avec `Scoped` se comportent comme des services `Singleton`.

- `singleton`une seule instance

- `transient`une instance du service est créée à chaque fois que le service est demandé.



## Ajout des `services`dans `Program.cs`

```cs
// ...

ConfigureServices(builder.Services);
// ...

void ConfigureServices(IServiceCollection services)
{
    services.AddSingleton<SingletonService>();
    services.AddTransient<TransientService>();
}
```



## Utilisation dans un composant `@inject`

```cs
@inject ServiceClassName serviceInstanceName
```

```cs
@inject SingletonService singleton
@inject TransientService transient
  
<p role="status">Singleton count: @singleton.Value</p>
<p role="status">Transient count: @transient.Value</p>
  
<button  @onclick="IncrementCount">Click me</button> 
  
@code {
   private void IncrementCount()
    {
        singleton.Value++;
        transient.Value++;
    }
}  
```

Si on change de page (démonte et remonte un composant), le service `transient` est remis à zéro, pas le service `singleton`.

<img src="../assets/service-singleton-vs-transient-ciunter.png" alt="service-singleton-vs-transient-ciunter" style="zoom:50%;" />



## Utilisation d'un `singleton`

La valeur d'un service `singleton` n'est pas perdu losqu'on navigue entre les pages (navigation interne seulement).

C'est un bon moyen de conserver les données entre les différentes parties de l'application.

`Index.razor`

```cs
@page "/"
@inject SingletonService singleton
// ...
  
<p role="status">Singleton count: @singleton.Value</p>  
```

<img src="../assets/singleton-persistence-against-component.png" alt="singleton-persistence-against-component" style="zoom:50%;" />

On conserve la valeur de la page `Counter`dans la page `Index`.

