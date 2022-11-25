# 01 Données stockées dans un fichier `Json`

## Données locales

On peut stocker ses données dans un fichier `json`.

```json
[
  {
    "customer": "Georges Henry",
    "items": [ { "name": "Iron Span" }, { "name": "Leather Ball" } ] 
  },
  { 
    "customer": "Phillipes Michelle",
    "items": [ { "name": "Ceramic Knife" } ]  
  }
]
```



On lit le fichier :

```cs
var jsonString = File.ReadAllText("orders.json");
```

On le parse :

```cs
var orders = JsonSerializer.Deserialize<IEnumerable<Order>>(jsonString);
```

Ici on risque d'avoir une erreur si on suit les conventions de nomage de chaque langage :

`c#` : `PascalCase`

`json` : `camelCase`

On doit ajouter une option pour indiquer au `Deserializer` cette convention :

```cs
var options = new JsonSerializerOptions {
    PropertyNamingPolicy = JsonNamingPolicy.CamelCase
};

var orders = JsonSerializer.Deserialize<IEnumerable<Order>>(json,options);
```

On peut simplement créer des `record` pour mapper les données du fichier `json` :

```cs
public record Order(string Customer, IEnumerable<Item> Items);

public record Item(string Name);
```

Il ne reste plus qu'à afficher les données :

```cs
foreach(var order in orders)
{
    WriteLine($"{order.Customer}");
    foreach(var item in order.Items)
    {
        WriteLine($"\t{item.Name}");
    }
}
```

```
Georges Henry
        Iron Span
        Leather Ball
Phillipes Michelle
        Ceramic Knife
```

