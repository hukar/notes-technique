# 03. Organisation des fichiers

Dans les `.csproj` des `classlib` on voit que c'est `netstandard2.0` qui est ciblé :

```csharp
<Project Sdk="Microsoft.NET.Sdk">

  <ItemGroup>
    <ProjectReference Include="..\Persistence\Persistence.csproj" />
  </ItemGroup>

  <PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
  </PropertyGroup>

</Project>
```

Il faudra peut-être ***upgradé*** manuellement la version du framework.

Puur `webapi`, on cible `netcoreapp3.1` :

```csharp
<Project Sdk="Microsoft.NET.Sdk.Web">

  <ItemGroup>
    <ProjectReference Include="..\Application\Application.csproj" />
  </ItemGroup>

  <PropertyGroup>
    <TargetFramework>netcoreapp3.1</TargetFramework>
  </PropertyGroup>


</Project>
```

## `API/Program.cs`

Point d'entrée de l'application :

Appelle `Starup.cs`

## `API/Startup.cs`

Fichier important où la configuration est chargée, où des services sont ajoutés et où des `middlewares` (`Use`) sont utilisés.

```csharp
public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }
```

les fichiers de configuration chargés sont `appsettings.json` et `appsettings.Development.json`.

`appsettings.json` est tout le temps chargé.

`appsettings.Development.json` n'est chargé qu'en `development`.

```csharp
	// This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddControllers();
        }
```

Injection de dépendances, c'est ici qu'on va ajouter nos services pour les rendre disponibles dans toute l'application.

```csharp
 // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            // app.UseHttpsRedirection();

            app.UseRouting();

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }
    }
```

`HTTP request pipeline` : ce sont les `middlewares` auxquels la requête va être passée.

L'ordre est important.

`app.UseDeveloperExceptionPage();` utilise une page d'erreur détaillée utile pendant le développement.

`app.UseHttpsRedirection();` désactivé pour le moment.

`app.UseEndpoints` Map les routes sur les contrôleurs.

## `API/Properties/launchSettings.json`

On va juste retiré le port pour `HTTPS` :

```json
"API": {
      "commandName": "Project",
      "launchBrowser": true,
      "launchUrl": "weatherforecast",
      // "applicationUrl": "https://localhost:5001;http://localhost:5000",
      "applicationUrl": "http://localhost:5000",
      "environmentVariables": {
        "ASPNETCORE_ENVIRONMENT": "Development"
      }
```

