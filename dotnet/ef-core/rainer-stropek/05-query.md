# 05 `Query` : Lire la `BDD`



## Récupérer certaine données

Les méthodes `Linq` sont utilisables et transformées en requête `SQL` par `EF Core`.

```cs
var dishesQuery = context.Dishes
    				.Where(d => d.Title.Contains("Porridge"));
```

Les requêtes `Linq` ne sont pas exécutés avant qu'on itère (`foreach`, `Count`, `Sum`, `ToList`, `ToArray`) sur leur résultat.

On va exécuter la requête avec `ToListAsync` :

```cs
var dishes = await dishesQuery.ToListAsync();
```



### Exécution de la requête

Une requête produisant un `singleton` est exécutée immédiatement :

- `Average`
- `Count`
- `Max`
- `First`

Pour obtebir le `singleton` la requête a besoin de connaître toute la séquence.

On peut aussi forcer l'exécution pour mettre en cache le résultat de la `Query` :

- `ToList`
- `ToDictionary`
- `ToArray`

On peut aussi utiliser une boucle, mais alors le résultat n'est pas sauvé en `cache`.

- `foreach`
- `ForEach`
- `for`

Les versions `Async` fonctionne pareil.