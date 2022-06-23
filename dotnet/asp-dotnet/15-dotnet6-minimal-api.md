# 15 `.net 6` et `Minimal API`





## `ConfigureServices` et `Configure`

Une bonne partie de ces réglages sont effectués par défaut.

Si on veut retrouver cette possibilité c'est très simple :

```cs
var builder = WebApplication.CreateBuilder(args);

// Configure Services : Add services to the container.
builder.Services.AddSwaggerGen(c =>
{
    c.SwaggerDoc("v1", new() { Title = "oldSchholStymleAPI", Version = "v1" });
});


var app = builder.Build();
// Configure : Configure the HTTP request pipeline.
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c => c.SwaggerEndpoint("/swagger/v1/swagger.json", "oldSchholStymleAPI v1"));
}
```



## Ajouter `Swagger`

Dans `MinimalApi.csproj`

```cs
 <ItemGroup>
    // ...
    <PackageReference Include="Swashbuckle.AspNetCore" Version="6.1.5" />
  </ItemGroup>
```



Ensuite dans `Program.cs`

```cs
var builder = WebApplication.CreateBuilder(args);

builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(c => {
    c.SwaggerDoc("v1", new() { Title = builder.Environment.ApplicationName, Version = "v1" });
});

var app = builder.Build();

if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI(c => c.SwaggerEndpoint("/swagger/v1/swagger.json", $"{builder.Environment.ApplicationName} v1"));
}
```

### Obtenir l'`Environment`

`app.Environment`

<img src="assets/environment-content.png" alt="environment-content"  />



## Les `alias` avec `using`

```cs
using foobar = Microsoft.AspNetCore....
```



## Configurer les `global using` par défaut

Dans le fichier `*.csproj`

```cs
<ItemGroup>
    <Using Include="Microsoft.AspNetCore.Mvc.*" Alias="Foobar" Condition="..."/>
</ItemGroup>
```

Cela supporte le caractère `*`.

On peut aussi utiliser l'attribut  `static` :

```cs
<ItemGroup>
    <Using Include="Microsoft.AspNetCore.Http.Results" Static="true"/>
</ItemGroup>
```

> `ms-build` est le système créant le `build` en utilisant le fichier `*.csproj` et le code source.



## Utiliser les méthodes d'extension pour organiser

On peut placer toutes les routes en relations dans une méthode d'extension :

`RoutesCustomer.cs`

```cs
namespace MinimalApi;

public static class RoutesCustomer
{
    public static void MapCustomer(this WebApplication app)
    {
        app.MapGet("/customer", () => "Hello customer");
        app.MapGet("customer/{id}", (int id) => $"customer {id}");
    }
}
```

Et ensuite dans `Program.cs` :

```cs
// ...
app.MapCustomer();

app.Run();
```



## Utiliser `appsettings.json` : `builder.Configuration`

Pour utiliser les configurations placées dans `appsettings.json` on a `builder.Configuration`.

```json
{
  "ConnectionStrings": {
    "HukarConnection": "Server=localhost,1433;Database=MinimalAPITest;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true"
  },
```

dans `Program.cs`

```cs
WriteLine(builder.Configuration["ConnectionStrings:HukarConnection"]);
//"Server=localhost,1433;Database=MinimalAPITest;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true"
```

