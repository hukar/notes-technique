# 01 Créer un projet `Blazor server`

## En `CLI`

```bash
dotnet new blazorserver --no-https -o MyProject
```

`--no-https` est l'équivalent de la décoche de `https` sur `Visual Studio 2019`.



## Les fichiers

### `Program.cs`

Toute application `asp.net core` est une application console qui va lancer un serveur capable de répondre aux requêtes `HTTP` : `Kestrel`.

On retrouve cela dans `Program.cs`.



### `Startup.cs`

`Startup.cs` est une classe de configuration :

- `ConfigureServices` qui va configurer l'injection de dépendances
- `Configure` qui va configurer le `pipeline` de `middleware`

```cs
public void ConfigureServices(IServiceCollection services)
{
  services.AddRazorPages();
  services.AddServerSideBlazor();
  services.AddSingleton<WeatherForecastService>();
}
```

On voit qu'on a des `Razor Pages` classique à disposition.

```cs
public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
{
  // ...

  app.UseStaticFiles();

  app.UseRouting();

  app.UseEndpoints(endpoints =>
                   {
                     endpoints.MapBlazorHub();
                     endpoints.MapFallbackToPage("/_Host");
                   });
}
```

`UseStaticFiles` ce sont les fichiers statiques contenus dans le dossier `wwwroot`.

`UseRouting` on a le routage à disposition.

`MapBlazorHub` Crée un `websocket` est ouvert grâce à `signalR` pour échanger des données du serveur au client.

`MapFallbackToPage("/_Host")` Sur une page inconnue, l'application retourne vers la page de base `/_Host`.