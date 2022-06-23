# 13 Méthode d'`extension`



On peut factoriser plusieurs `Endpoints` en relation, grâce aux `Extension Methods`.

```cs
public static class ContactsApiExtension
{
    public static WebApplication MapContactsApi(this WebApplication app)
    {
        app.MapGet(
            "/contacts", 
            (ContactRepository repository) 
                => Results.Ok(repository.Get())
        );

        return app;
    }
}
```

On retourne `app` : `WebApplication` pour garder le côté `Fluent` à la méthode.

On a maintenant dans `Program.cs`

```cs
app.MapContactsApi();
```

On peut faire de même avec le service de `ContactRepository` et le placer dans le même fichier :

```cs
public static class ContactsApiExtension
{
    public static IServiceCollection UseContactsApi
    (this IServiceCollection services)
    {
        services.AddTransient<ContactRepository>();

        return services;
    }
    
    // ... MapContactApi
```

On veut garder le côté `Fluent Api` en renvoyant `services`, on garde la possibilité de chaîner les méthodes.

> `Fluent Api` désigne le fait de pouvoir chaîner les méthodes :
>
> ```cs
> app.DoSomething().WithAuthorization().ToLogger()... ;					
> ```
>
> On renvoie dans chaque méthode l'objet principal ici `app`.

On a dans `Program.cs`

```cs
var builder = WebApplication.CreateBuilder(args);

builder.Services.UseContactsApi();

var app = builder.Build();

app.MapContactsApi();
```

### Par convention

`UseSomething` pour ajouter une configuration.

`MapSomething` pour ajouter un comportement.