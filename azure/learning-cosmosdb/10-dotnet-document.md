# 10 Manipulation des `Document`

## Créer un `Document`

### À partir d'un objet `dynamic`

```csharp
async static Task CreateDocuments()
{
  Console.Clear();
  Console.WriteLine(">>> Create Documents <<<");

  var container = Shared.Client.GetContainer("Users", "Families");

  dynamic document1Dynamic = new
  {
    id = "Raymond",
    guid = Guid.NewGuid(),
    address = new
    {
      addressLine1 = "78 sésame Street",
      location = new
      {
        city = "Brooklyn",
        stateProvinceName = "New York"
      },
      countryRegionName = "United States"
    }
  };

  await container.CreateItemAsync(document1Dynamic);
  Console.WriteLine($"Created new document {document1Dynamic.id} from dynamic objet");
```

`Console.Clear()` néttoie la console.

`Guid.NewGuid()` génère un identifiant unique. Ne nécessite pas de `using`.

`dynamic` permet de typer de dynamiquement : à l'exécution.

`new { ... }` permet de créer un objet litéral (sans classe).

### `c.CreateItemAsync(<T> item)`



### À partir d'un `json string`

```csharp
var document2Json = $@"
            {{
                ""id"": ""Henry"",
                ""guid"": ""{Guid.NewGuid()}"",
                ""address"": {{
                    ""addressLine1"": ""78 sésame Street"",
                    ""location"": {{
                        ""city"": ""Brooklyn"",
                        ""stateProvinceName"": ""New York""
                    }},
                    ""postalCode"": ""11229"",
                }}
            }}";

var document2Object = JsonConvert.DeserializeObject<JObject>(document2Json);
await container.CreateItemAsync(document2Object);
Console.WriteLine($"Created new document {document2Object["id"].Value<string>()} from JSON string");
```

Méthode à la syntaxe un peu rude.

`@"..."`  : Tout ce qui dans la chaîne, serait normalement interprété comme une séquence d'échappement, est ignoré.

> `"\”hey you\""` => `@""hey you""`

Il faut : 

```csharp
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
```

#### `JsonConvert.DeserializeObject<JObject>(document2Json)`



### À partir d'un objet `c#`

On crée deux classes pour représenter nos données.

Ce sont des classes ordinaires qui peuvent aussi avoir des `methods`.

`Person.cs`

```csharp
using Newtonsoft.Json;

namespace azure_cosmos_connection
{
    public class Person
    {
        [JsonProperty(PropertyName = "id")]
        public string Id { get; set; }

        [JsonProperty(PropertyName = "address")]
        public string Address { get; set; }

        [JsonProperty(PropertyName = "pet")]
        public Pet MyPet { get; set; }
    }

    public class Pet
    {
        [JsonProperty(PropertyName = "name")]
        public string Name { get; set; }

        public string Warf()
        {
            return "warf warf";
        }
    }
}
```

On utilise `[JsonProperty(PropertyName = "camelCase")]` car les membres en `c#` sont eux en `PascalCase`.

Dans `Program.cs`

```csharp
var document3Poco = new Person
{
  Id = "Marie",
  Address = "Canaby Street 567, 67895 Ohio USA",
  MyPet = new Pet
  {
    Name = "Popaw"
  }
};

Console.WriteLine(document3Poco.MyPet.Warf());

await container.CreateItemAsync(document3Poco);
Console.WriteLine($"Created new document {document3Poco.Id} from typed object");
```

Le résultat :

```json
{
    "id": "Marie",
    "address": "Canaby Street 567, 67895 Ohio USA",
    "pet": {
        "name": "Popaw"
    },
    "_rid": "IOJTAJUl-AMIAAAAAAAAAA==",
    "_self": "dbs/IOJTAA==/colls/IOJTAJUl-AM=/docs/IOJTAJUl-AMIAAAAAAAAAA==/",
    "_etag": "\"83003032-0000-0e00-0000-6022a9740000\"",
    "_attachments": "attachments/",
    "_ts": 1612884340
}
```

À part l'utilisation de `[JsonProperty()]`, la transformation en `Json` est totalement transparente.

Les classes peuvent avoir des `méthodes`, celle-ci ne seront pas reprise dans le `Json` mais ne provoqueront pas d'erreur non plus.



## Requêter les `Documents`

### Objet `dynamic`

```cs
async static Task QueryDocuments()
{
  System.Console.WriteLine("\n>>> Query Documents (SQL) <<<\n");

  // On récupère le container
  var container = Shared.Client.GetContainer("Users", "Families");

  var sql = "SELECT * FROM c WHERE STARTSWITH(c.id, 'Ma') = true";

  // Requête pour un objet dynamic
  var iteratorDynamic = container.GetItemQueryIterator<dynamic>(sql);
  var documentsDynamic = await iteratorDynamic.ReadNextAsync();

  System.Console.WriteLine($"Retrieved {documentsDynamic.Count} documents");
  foreach (var doc in documentsDynamic)
  {
    System.Console.WriteLine(doc);
    System.Console.WriteLine("- - - - - - - - ");

    // Conversion de l'objet dynamic dans un type défini
    // attention l'objet json doit avoir les mêmes propriétés que le type de conversion
    var person = JsonConvert.DeserializeObject<Person>(doc.ToString());
    System.Console.WriteLine($"person transformed in typed object : {person.Id}");
    System.Console.WriteLine("----------\n");
  }
```

Résultat :

```bash
>>> Query Documents (SQL) <<<

Retrieved 1 documents
{
  "id": "Marie",
  "address": "Canaby Street 567, 67895 Ohio USA",
  "pet": {
    "name": "Popaw"
  },
  "_rid": "IOJTAJUl-AMIAAAAAAAAAA==",
  "_self": "dbs/IOJTAA==/colls/IOJTAJUl-AM=/docs/IOJTAJUl-AMIAAAAAAAAAA==/",
  "_etag": "\"83003032-0000-0e00-0000-6022a9740000\"",
  "_attachments": "attachments/",
  "_ts": 1612884340
}
- - - - - - - - 
person transformed in typed object : Marie
----------
```

#### `WHERE STARTSWITH(c.id, 'Ma') = true` qui commence par ...

#### `GetItemQueryIterator<dynamic>(sql)` renvoie un `iterator`

`JsonConvert.DeserializeObject<Person>(doc.ToString())` converti en type défini l'objet de type `dynamic`.

> ! L'objet `json` reçu doit correspondre au modèle du type (mêmes champs).



### Requête pour un type défini : `Person`

```csharp
// Requête pour un type défini : Person
var iteratorTyped = container.GetItemQueryIterator<Person>(sql);
var documentsTyped = await iteratorTyped.ReadNextAsync();
System.Console.WriteLine($"There are {documentsTyped.Count} documents retrieved");
foreach (var person in documentsTyped)
{
  System.Console.WriteLine($"name: {person.Id}, address: {person.Address}, pet: {person.MyPet.Name}");
}
```

On type l'`iterator` retourné : `GetItemQueryIterator<Person>`.

On n'a plus besoin de l'étape de conversion, l'`iterator` renvoie des objets déjà typés.

On remarque dans le `Console.WriteLine` que les membres appelés sont en `PascalCase`, ce sont ceux d'un objet `c#`.

```bash
There are 1 documents retrieved
name: Marie, address: Canaby Street 567, 67895 Ohio USA, pet: Popaw
```





## `code` complet

`Person.cs`

```csharp
using Newtonsoft.Json;

namespace azure_cosmos_connection
{
    public class Person
    {
        [JsonProperty(PropertyName = "id")]
        public string Id { get; set; }

        [JsonProperty(PropertyName = "address")]
        public string Address { get; set; }

        [JsonProperty(PropertyName = "pet")]
        public Pet MyPet { get; set; }
    }

    public class Pet
    {
        [JsonProperty(PropertyName = "name")]
        public string Name { get; set; }

        public string Warf()
        {
            return "warf warf";
        }
    }
}
```

`Program.cs`

```csharp
using System;
using System.Threading.Tasks;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace azure_cosmos_connection
{
    class Program
    {
        async static Task Main(string[] args)
        {
            // await CreateDocuments();
            await QueryDocuments();
        }

        async static Task ViewDocuments()
        {
            using (var client = Shared.Client)
            {
                var container = client.GetContainer("Users", "Families");
                var sql = "SELECT * FROM c";
                var iterator = container.GetItemQueryIterator<dynamic>(sql);

                var page = await iterator.ReadNextAsync();

                foreach (var doc in page)
                {
                    Console.WriteLine(doc.id);
                }
            }
        }

        async static Task CreateDocuments()
        {
            Console.Clear();
            Console.WriteLine(">>> Create Documents <<<");
            Console.WriteLine();

            var container = Shared.Client.GetContainer("Users", "Families");

            dynamic document1Dynamic = new
            {
                id = "Raymond",
                guid = Guid.NewGuid(),
                name = "New Customer 1",
                address = new
                {
                    addressType = "Main Office",
                    addressLine1 = "78 sésame Street",
                    location = new
                    {
                        city = "Brooklyn",
                        stateProvinceName = "New York"
                    },
                    postalCode = "11229",
                    countryRegionName = "United States"
                }
            };

            // await container.CreateItemAsync(document1Dynamic);
            Console.WriteLine($"Created new document {document1Dynamic.id} from dynamic objet");

            var document2Json = $@"
            {{
                ""id"": ""Henry"",
                ""guid"": ""{Guid.NewGuid()}"",
                ""name"": ""New Customer 1"",
                ""address"": {{
                    ""addressType"": ""Main Office"",
                    ""addressLine1"": ""78 sésame Street"",
                    ""location"": {{
                        ""city"": ""Brooklyn"",
                        ""stateProvinceName"": ""New York""
                    }},
                    ""postalCode"": ""11229"",
                    ""countryRegionName"": ""United States""
                }}
            }}";

            var document2Object = JsonConvert.DeserializeObject<JObject>(document2Json);
            // await container.CreateItemAsync(document2Object);
            Console.WriteLine($"Created new document {document2Object["id"].Value<string>()} from JSON string");

            var document3Poco = new Person
            {
                Id = "Marie",
                Address = "Canaby Street 567, 67895 Ohio USA",
                MyPet = new Pet
                {
                    Name = "Popaw"
                }
            };

            Console.WriteLine(document3Poco.MyPet.Warf());

            await container.CreateItemAsync(document3Poco);
            Console.WriteLine($"Created new document {document3Poco.Id} from typed object");
        }

        async static Task QueryDocuments()
        {
            System.Console.WriteLine("\n>>> Query Documents (SQL) <<<\n");

            // On récupère le container
            var container = Shared.Client.GetContainer("Users", "Families");

            var sql = "SELECT * FROM c WHERE STARTSWITH(c.id, 'Ma') = true";

            // Requête pour un objet dynamic
            var iteratorDynamic = container.GetItemQueryIterator<dynamic>(sql);
            var documentsDynamic = await iteratorDynamic.ReadNextAsync();

            System.Console.WriteLine($"Retrieved {documentsDynamic.Count} documents");
            foreach (var doc in documentsDynamic)
            {
                System.Console.WriteLine(doc);
                System.Console.WriteLine("- - - - - - - - ");

                // Conversion de l'objet dynamic dans un type défini
                // attention l'objet json doit avoir les mêmes propriétés que le type de conversion
                var person = JsonConvert.DeserializeObject<Person>(doc.ToString());
                System.Console.WriteLine($"person transformed in typed object : {person.Id}");
                System.Console.WriteLine("----------\n");
            }
            System.Console.WriteLine();

            // Requête pour un type défini : Person
            var iteratorTyped = container.GetItemQueryIterator<Person>(sql);
            var documentsTyped = await iteratorTyped.ReadNextAsync();
            System.Console.WriteLine($"There are {documentsTyped.Count} documents retrieved");
            foreach (var person in documentsTyped)
            {
                System.Console.WriteLine($"name: {person.Id}, address: {person.Address}, pet: {person.MyPet.Name}");
            }
        }
    }
}
```

