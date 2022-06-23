# 04 `Command` : modifier la `BDD`

## Ajouter une donnée : `Add(item)`

```cs
var pooridge = new Dish { Title = "Pizza", Notes = "My favourite dish in the world", Stars = 5 };

context.Dishes.Add(pooridge);
```

Ajouter, supprimer ou modifier un élément créé une `transaction`.

la `BDD` n'est pas tout de suite modifiée, il faut appeler une méthode très importante :

### `context.SaveChangesAsync()`

```cs
await context.SaveChangesAsync();
```

`Add` enregistre l'objet comme candidat à être copié en `BDD` lors du prochain appelle à `SaveChangesAsync`.



## Récupération de l'`Id`

De manière magique lorsqu'on appelle `SaveChangesAsync`, notre objet est enregistré en `BDD`, automatiquement un `Id` lui est attribué et est renvoyé vers l'objet de notre programme !

```cs
var pooridge = new Dish { ... };

context.Dishes.Add(pooridge);
await context.SaveChangesAsync();
Console.WriteLine($"Dish with Id :{pooridge.Id} was inserted successfuly in DB");
```

```
Dish with Id :3 was inserted successfuly in DB
```

Les `Id` effacés ne sont pas réutilisés.



## Supprimer une donnée : `Delete`

```cs
context.Dishes.Remove(pooridge);
await context.SaveChangesAsync();
```



## Mettre à jour une donnée

```cs
pooridge.Stars = 5;
await context.SaveChangesAsync();
```

`EF Core` track les éléments et les marque comme modifié lors d'une assignation.

Il suffit alors de faire un `SaveChangesAsync`.



## `Update` de toutes les propriétés

```cs
public async Task UpdateCompany(Company companyUpdated)
{
    _db.Entry(companyUpdated).State = EntityState.Modified;
    await _db.SaveChangesAsync();
}
```

Cela ne met pas à jour les objets imbriqués, il faut alors boucler dessus :

```cs
public async Task UpdateCompany(Company companyUpdated)
{
    _db.Entry(companyUpdated).State = EntityState.Modified;
    foreach(var car in company.CompanyCars)
    {
        db.Entry(car).State = EntityState.Modified;
    }
    await _db.SaveChangesAsync();
}
```





## Requêter les données : `Querying Data`

Les opérations de lecture permettent de filtrer les données souhaitées.