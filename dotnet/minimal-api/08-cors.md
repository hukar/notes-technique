# 08 `cors`

## Mise en place `AddCors` et `UseCors`

Il faut ajouter un `service` : `AddCors` et un `middleware` : `UseCors`

```cs
builder.Services.AddCors(options =>
{
    options.AddPolicy(name: "MyAllowSpecificOrigins",
                      policy  =>
                      {
                          policy.WithOrigins("http://localhost:5135")
                                .AllowAnyMethod();
                      });
});

// ...

app.UseCors();
```

Les `Urls` sont celles du `Client`.

`policy` est de type `CorsPolicyBuilder` et a plusieurs méthode :

```cs
policy.AllowAnyOrigin();
policy.WithMethods("GET");
policy.AllowAnyHeader();
```

<img src="assets/options-methods-cors-policy-builder-one.png" alt="options-methods-cors-policy-builder-one" style="zoom:33%;" />

<img src="assets/cors-policy-builder-options-available-two.png" alt="cors-policy-builder-options-available-two" style="zoom:33%;" />

## Utilisation `RequireCors`

Pour utiliser la `policy` sur un `endpoint` on utilise la méthode `RequireCors`.

```cs
app.MapGet("/", () => {
        // ...
    })
    .RequireCors(MyAllowSpecificOrigins);
```

Ou bien on a cofiguré `policy.AllowAnyMethod()` et là cette `policy` est directement valable pour toutes les méthodes.
