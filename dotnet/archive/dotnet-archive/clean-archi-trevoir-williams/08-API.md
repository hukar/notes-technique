# `API`

Création de l'`API`

```bash
dotnet new webapi -o API
```



## `appsettings.json`

On doit ajouter les informations de configuration dans ce fichier pour `SendMail` et pour `EF Core` :

```cs
{
    "ConnectionStrings": {
        "LeaveManagementConnectionString" : "Server=localhost,1433; Database=hr_leave_management; User=sa; Password=huk@r2Xmen99"
    },
    "EmailSettings" : {
        "ApiKey" : "SENDGRID_KEY_API",
        "FromName": "Leave Management System",
        "FromAddress": "noreply@leavemanagement.com"
    },
    "Logging": {
    "LogLevel": {
      "Default": "Information",
      "Microsoft": "Warning",
      "Microsoft.Hosting.Lifetime": "Information"
    }
  },
  "AllowedHosts": "*"
}
```



## `Startup.cs`

On va enregistrer les services des autres couches :

```cs
public void ConfigureServices(IServiceCollection services)
{

  services.AddApplicationServices();
  services.AddInfrastructureServices(Configuration);
  services.AddPersistenceServices(Configuration);
  services.AddControllers();
```

Il faut bien sûr ajouter au projet des `references` vers les autres projets `Application`, `Persistence` et `Infrastructure`.

```bash
dotnet add API reference Persistence 

dotnet add API reference Infrastructure

dotnet add API reference Application
```

Il ne faut pas oublier, lorsque nécéssaire, de passer la propriété `Configuration`.



## `CORS policy`

Configuration des `cors` :

```cs
services.AddCors(o => {
  o.AddPolicy("CorsPolicy", builder => builder.AllowAnyOrigin()
             	.AllowAnyMethod()
             	.AllowAnyHeader());
});
```

Et on l'ajoute au pipeline de `middleware` :

```cs
app.UseAuthorization();

app.UseCors("CorsPolicy");

app.UseEndpoints(endpoints => {
  endpoints.MapControllers();
});
}
```



## `Migration`

On va maintenant créer la `BDD` , on doit mettre à jour `dotnet-ef` et installer `EntityFrameworkCore.Design` dans le projet `Persistence` :

```bash
dotnet tool update --global dotnet-ef

cd Persistence

dotnet add package Microsoft.EntityFrameworkCore.Design --version 5.0.10
```

On lance la migration dans le projet `Persistence`.

```bash
dotnet ef migrations add FirtsMigration -s ../API
```

`-s` `--startup` désigne le projet `Startup`.



### Appliquer la `migration`

```bash
✨ Persistence : dotnet ef database update -s ../API 
Build started...
Build succeeded.
Done.
```

De nouveau on doit spécifier le projet `Startup`.

