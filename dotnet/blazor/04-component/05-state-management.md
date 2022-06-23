# 05 Gestion de l'état

> On ne doit pas dépendre de classe réel mais d'abstraction (d'`interface`).
>
> C'est le principe d'inversion de dépendance.

## Par Injection de dépendances

Utilisation conjointe de l'injection de dépendances et de l'utilisation d'une interface `IRepository`.

### Création du `IRepository`

```cs
public Interface IRepository
{
  List<Movie> GetMovies();
}
```



## Implémentation dans la classe `RepositoryInMemory`

```cs
public class RepositoryInMemory : IRepository
{
    public List<Movie> GetMovies() => new List<Movie>() {/* ... */};
}
```



## Ajout du `Repository`comme service

`Program.cs`

```cs
void ConfigureServices(IServiceCollection services)
{
  services.AddTransient<IRepository,RepositoryInMemory>();
```



## Utilisation du service

```cs
@inject IRepository repository
  
// ...
  
@code {
  List<Movie>? movies;
  
  protected override void OnInitialized()
  {
    movies = repository.GetMovies();
  }
}  
```

