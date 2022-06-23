# 03 `cors`

## `C`ross `O`rigin `R`essource `S`haring

Si un utilisateur en dehors du domaine de `l'API` essaye d'accéder à une ressource de l'`API`,

par défaut sa demande est rejetée.

Il faut configurer une politique de `CORS`.

Cela se fait dans le fichier `Startup.cs` :

```cs
public void ConfigureServices(ServiceCollection services)
{
  // ...
  
  services.AddCors(o => {
    o.AddPolicy("AllowAllPolicy", builder => 
               builder.AllowAnyOrigin()
               	.AllowAnyMethod()
               	.AllowAnyHeader());
  });
}
```

Plus bas dans la méthode `Configure` :

```cs
public void Configure(IApplicationBuilder app, IWebhostEnvironment env)
{
  // ...
  
  app.UseHttpRedirection();
  
  app.UseCors("AllowAllPolicy");
}
```

`AddPolicy(<Policy Name>, builder)` : le nom de la politique est libre de choix, ici c'est une politique très permissive autorisant toutes les `origin`, toutes les `Method` et tous les `Header`.

