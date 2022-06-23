# 07 Programmer avec le `SDK .net` : `SELECT`

## Getting started

On va créer un projet console :

```bash
dotnet new console
```

On Doit aussi ajouter les package :

`Microsoft.Azure.Cosmos` et `Microsoft.Extensions.Configuration.Json` pour mettre sa config dans un fichier `json`.

```bash
dotnet add package Microsoft.Extensions.Configuration.Json
dotnet add package Microsoft.Azure.Cosmos
```

On va créer un fichier de config : `appsettings.json`

```json
{
    "CosmosEndpoint": "https://hukarcosmos.documents.azure.com:443/",
    "CosmosMasterKey": "SyZQ05PPRAEDcFgFdqd9vADjCNZmhqp03a6kaUlAmcm8s9whyPPpYpEcENICepwhDwD1aEZGAVmlOLLskM5Bpg=="
}
```

#### ! Pour que le fichier de config soit vu il faut ajouter dans le fichier `.csproj` :

```c#
<ItemGroup>
    <None Update="appsettings.json">
    	<CopyToOutputDirectory>PreserveNewest</CopyToOutputDirectory>
    </None>
</ItemGroup>
```



Dans `Program.cs` :

```c#
using System;
using System.Threading.Tasks;
using Microsoft.Azure.Cosmos;
using Microsoft.Extensions.Configuration;

namespace azure_cosmos_connection
{
    class Program
    {
        async static Task Main(string[] args) => await QueryForDocuments();

        async static Task QueryForDocuments()
        {
            var config = new ConfigurationBuilder().AddJsonFile("appsettings.json").Build();
            var endpoint = config["CosmosEndpoint"];
            var masterKey = config["CosmosMasterKey"];

            using (var client = new CosmosClient(endpoint, masterKey))
            {
                var container = client.GetContainer("Users", "Families");
                var sql = "SELECT * FROM c WHERE ARRAY_LENGTH(c.children) > 1";
                var iterator = container.GetItemQueryIterator<dynamic>(sql);

                var page = await iterator.ReadNextAsync();

                foreach(var doc in page)
                {
                    Console.WriteLine($"Family {doc.id} has {doc.children.Count} children");
                }
            }
        }
    }
}
```

`using` permet de fermer proprement les connexions automatiquement si une erreur est levée.

> **Stackoverflow :**
>
> Il ne peut être utilisé qu'avec les types qui mettent en œuvre `IDisposable`, et est le sucre syntaxique pour un bloc `try/finally` qui appelle `Dispose` dans le `finally bloc`. 
>
> Ceci est utilisé pour simplifier la gestion des ressources.

`<dynamic>` on pourrait mettre un type présent dans notre projet, mais ici avec `dynamic` on accepte les enregistrement de n'importe quel type.

Tout comme en javascript, une méthode ayant un élément avec `await` devant, doit avoir le mot `async` dans sa signature.

Au lieu de renvoyer `void`, une fonction `async` renvoie une `Task` (équivalent de la promesse javascript).

#### ! en `c#` les méthodes et mes propriétés ont une première lettre en capitale :

`Build()` dans  `new ConfigurationBuilder().AddJsonFile("appsettings.json").Build()` ou `Count` dans `doc.children.Count`.

## Créer une seule instance de `client` pour le projet

C'est une bonne pratique que de créer une seule instance de `client` pour tout le projet.

ON va créer une classe `static` pour partager le `Client`.

`Shared.cs`

```cs
using Microsoft.Azure.Cosmos;
using Microsoft.Extensions.Configuration;

namespace azure_cosmos_connection
{
    public class Shared
    {
        public static CosmosClient Client { get; private set; }

        static Shared()
        {
            var config = new ConfigurationBuilder().AddJsonFile("appsettings.json").Build();
            var endpoint = config["CosmosEndpoint"];
            var masterKey = config["CosmosMasterKey"];

            Client = new CosmosClient(endpoint, masterKey);
        }
    }
}
```

Notre `Program.cs` est du coup plus simple :

```cs
using System;
using System.Threading.Tasks;

namespace azure_cosmos_connection
{
    class Program
    {
        async static Task Main(string[] args) => await QueryForDocuments();

        async static Task QueryForDocuments()
        {
            using (var client = Shared.Client)
            {
                var container = client.GetContainer("Users", "Families");
                var sql = "SELECT * FROM c WHERE ARRAY_LENGTH(c.children) > 1";
                var iterator = container.GetItemQueryIterator<dynamic>(sql);

                var page = await iterator.ReadNextAsync();

                foreach (var doc in page)
                {
                    Console.WriteLine($"Family {doc.id} has {doc.children.Count} children");
                }
            }
        }
    }
}
```

