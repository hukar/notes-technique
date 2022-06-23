# EE Liste imbriquée d'objets

## `Include` et `Select`

<img src="assets/representative-demander-n-m-relationship.png" alt="representative-demander-n-m-relationship" style="zoom:50%;" />



### `Include`

On souhaite obtenir les `Representatives` avec leurs `Demanders` :

```cs
await _db.Representatives
    .Include(r => r.Demanders)
    .ToListAsync();
```

`Résultat json`

```json
[
  {
    "id": 1,
    "name": "Charles Ingeals",
    "demanders": [
      {
        "id": 1,
        "name": "Raymond D'Or",
        "representatives": [
          null
        ]
      },
      {
        "id": 2,
        "name": "Pascal Brutal",
        "representatives": [
          null,
          {
            "id": 2,
            "name": "Paula Abdul",
            "demanders": [
              null,
              {
                "id": 3,
                "name": "Leia Organa",
                "representatives": [
                  null
                ]
              }
            ]
          }
        ]
      }
    ]
  },
  {
    "id": 2,
    "name": "Paula Abdul",
    "demanders": [
      {
        "id": 2,
        "name": "Pascal Brutal",
        "representatives": [
          {
            "id": 1,
            "name": "Charles Ingeals",
            "demanders": [
              {
                "id": 1,
                "name": "Raymond D'Or",
                "representatives": [
                  null
                ]
              },
              null
            ]
          },
          null
        ]
      },
      {
        "id": 3,
        "name": "Leia Organa",
        "representatives": [
          null
        ]
      }
    ]
  },
  {
    "id": 3,
    "name": "Richard III",
    "demanders": []
  }
]
```

On obtient un résultats compliqués à cause de la navigation circulaire :

 `representative-> demanders -> representatives -> ...`

> Le `json` est généré grâce à ce réglage dans `Program.cs` :
>
> ```cs
> using System.Text.Json.Serialization;
> using Microsoft.AspNetCore.Http.Json;
> 
> // ...
> 
> builder.Services.Configure<JsonOptions>(
>     options => options.SerializerOptions.ReferenceHandler = ReferenceHandler.IgnoreCycles
> );
> ```
>
> Cela évite une erreur de parsage `Circular Reference`.



### `Select`

Pour éviter ce `json` compliqué, on souhaite faire une projection avec l'aide de `DTO`.

On peut utiliser les `record` pour créer facilement des `DTO` :

```cs
private record RepresentativeDto(int Id, string Name, List<DemanderDto> Demanders);

private record DemanderDto(int Id, string Name);
```

On utilise maintenant la projection avec `Select` pour notre requête :

```cs
await _db.Representatives
    .Select(r => new RepresentativeDto(
        r.Id,
        r.Name,
        r.Demanders.Select(d => new DemanderDto(d.Id, d.Name)).ToList()
    ))
    .ToListAsync();
```

Le résultat (parsé en `json`) est maintenant très propre :

```json
[
  {
    "id": 1,
    "name": "Charles Ingeals",
    "demanders": [
      {
        "id": 1,
        "name": "Raymond D'Or"
      },
      {
        "id": 2,
        "name": "Pascal Brutal"
      }
    ]
  },
  {
    "id": 2,
    "name": "Paula Abdul",
    "demanders": [
      {
        "id": 2,
        "name": "Pascal Brutal"
      },
      {
        "id": 3,
        "name": "Leia Organa"
      }
    ]
  },
  {
    "id": 3,
    "name": "Richard III",
    "demanders": []
  }
]
```



