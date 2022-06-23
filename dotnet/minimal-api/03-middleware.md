# 02 Les `Middleware`

## Ajouter un `Middleware`

`Program.cs`

```cs
var builder = WebApplication.CreateBuilder(args);

// Ajouter les services builder.Services.Add...

var app = builder.Build();

app.UseHttpsRedirection();

app.UseStaticFiles();
// ou app.UseFileServer(); qui est plus complet

app.Run();
```

