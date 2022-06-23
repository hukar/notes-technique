# Les dossiers et fichiers d'une `webapi`

## Configuration

`appsettings.Development.json`

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Information",
      // "Microsoft": "Warning",
      "Microsoft": "Information",
      "Microsoft.Hosting.Lifetime": "Information"
    }
  }
}
```

`"Microsoft": "Information"` ce changement permet d'avoir plus de détails dans les logs console.

## `Startup.cs`

```csharp
 public class Startup
 {
     public Startup(IConfiguration configuration)
     {
         Configuration = configuration;
     }

     public IConfiguration Configuration { get; }

     // This method gets called by the runtime. Use this method to add services to the container.
     public void ConfigureServices(IServiceCollection services)
     {
         services.AddControllers();
         services.AddSwaggerGen(c =>
            {
                c.SwaggerDoc("v1", new OpenApiInfo { Title = "API", Version = "v1" });
             });
     }

     // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
     public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
     {
         if (env.IsDevelopment())
         {
             app.UseDeveloperExceptionPage();
             app.UseSwagger();
             app.UseSwaggerUI(c => c.SwaggerEndpoint("/swagger/v1/swagger.json", "API v1"));
         }

         app.UseHttpsRedirection();

         app.UseRouting();

         app.UseAuthorization();

         app.UseEndpoints(endpoints =>
         {
           endpoints.MapControllers();
         });
     }
 }
```



`Startup(IConfiguration configuration)` le constructeur récupère les configuration par injection de dépendance :

- `appsettings.Development.json`
- `appsettings.json` global

`ConfigureServices` méthode gérant l'injection de dépendances pour le conteneur à services.

`Configure` c'est le pipeline de la requête `HTTP`. C'est là où les `middlewares` entre la `request` et la `response` vont être appliqués.

## `Properties/launchSettings.json`

```json
"API": {
      "commandName": "Project",
      "dotnetRunMessages": "true",
      // "launchBrowser": true,
      "launchBrowser": false,
      "launchUrl": "swagger",
      "applicationUrl": "https://localhost:5001;http://localhost:5000",
      "environmentVariables": {
        "ASPNETCORE_ENVIRONMENT": "Development"
      }
```

On peut mettre `lauchBrowser` à `false` comme c'est une `API`.

On défini ici aussi les variables d'environnement.

## Utiliser un `watcher`

```bash
dotnet watch run --project API
```



## `WeatherForecastController`

Le contrôleur est précédé d'`attribute` lui conférrant de *super pouvoir*.

```cs
using // ...

namespace API.Controllers
{
    [ApiController]
    [Route("[controller]")]
    public class WeatherForecastController : ControllerBase
    {
        private static readonly string[] Summaries = new[]
        {
            "Freezing", "Bracing", "Chilly", "Cool", "Mild", "Warm", "Balmy", "Hot", "Sweltering", "Scorching"
        };

        private readonly ILogger<WeatherForecastController> _logger;

        public WeatherForecastController(ILogger<WeatherForecastController> logger)
        {
            _logger = logger;
        }

        [HttpGet]
        public IEnumerable<WeatherForecast> Get()
        {
            var rng = new Random();
            return Enumerable.Range(1, 5).Select(index => new WeatherForecast
            {
                Date = DateTime.Now.AddDays(index),
                TemperatureC = rng.Next(-20, 55),
                Summary = Summaries[rng.Next(Summaries.Length)]
            })
            .ToArray();
        }
    }
}
```

`[Route("[controller]")]` prende comme nom de route le nom du contrôleur sans le suffixe `controller`.

`[HttpGet]` répond à une requête `GET`.