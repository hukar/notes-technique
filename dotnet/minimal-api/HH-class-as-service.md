# HH Architecture : Une classe comme service

## Le problème

Avec `minimal api`, chaque `RequestDelegate` doit recevoir ses propres services.

L'injection de dépendances se fait dans les paramètres :

```cs
app.MapGet("/helloworld", (AppRepository repo) => { ... });
```



## La solution

Pour centraliser les dépendances (injection des services) on peut utiliser une classe :

```cs
public class CompanyEndpoints
{
    private readonly IAsyncRepository _repo;
    public CompanyEndpoints(IAsyncRepository repo)
    {
        _repo = repo;    
    }

    public void RegisterCompanyEndpoints(WebApplication app)
    {

        app.MapGet("/company", async () => {
            var companies = await _repo.GetAllCompanies();
            return Ok(companies);
        });

        app.MapGet("/company/{id:int}", async (int id) 
            => await _repo.GetCompanyById(id) is Company company ? 
                   Ok(company) : NotFound());

        app.MapPost("/company", async (Company company) => {
            await _repo.CreateCompany(company);
            return Created($"/Company/{company.CompanyId}", company);
        });

        app.MapPut("/company", async (Company company) => {
            await _repo.UpdateCompany(company);
            return NoContent();
        });

        app.MapDelete("/company/{id:int}", async (int id) => {
            await _repo.DeleteCompany(id);
            return NoContent();
        });
    }
}
```

Le `_repo` peut ainsi être assigné une seule fois dans le constructeur et non pas dans chaque `RequestDelegate`.



## Utilisation dans `Program.cs`

On ajoute la classe `CompanyEndpoints` comme `service`

```cs
builder.Services.AddScoped<CompanyEndpoints>();
```

Comme c'est un `service` `scoped`, on doit créer un `scope` :

```cs
using var scope = app.Services.CreateScope();
```

Puis retrouver le service via ce `scope`

```cs
var companyEndpoints = scope.ServiceProvider.GetRequiredService<CompanyEndpoints>();
```

Enfin on passe l'objet `WebApplication` et on lance l'application :

```cs
companyEndpoints.RegisterCompanyEndpoints(app);

app.Run();
```

