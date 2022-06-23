# 11 Pagination `stateful` et `stateless`

## `stateful`

Par défaut `iterator.ReadNexttAsync()` renvoie maximum `1000` `documents` par `page`.

On peut modifier ce nombre quand on crée l'`iterator` : 

#### `GetItemQueryIterator<T>(sql, requestOptions: new QueryRequestOptions { MaxItemCount = 100 })`

```csharp
// Les cent premier document
var container = Client.GetContainer("Users", "Families");

var sql = "SELECT * FROM c";

var iterator = container.GetItemQueryIterator<Person>(sql, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });
var page = await iterator.ReadNextAsync();

System.Console.WriteLine($"There are {page.Count} documents retrieved");
foreach (var person in page)
{
  System.Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
}
```



### Récupérer tous les documents

Il suffit de mettre en place un `while(iterator.HasMoreResults)`:

### `iterator.HasMoreResults`

```csharp
// Get all pages using iterator.HasMoreResults
iterator = container.GetItemQueryIterator<Person>(sql, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });

var itemCount = 0;
var pageCount = 0;

while (iterator.HasMoreResults)
{
  pageCount++;
  page = await iterator.ReadNextAsync();
  System.Console.WriteLine($"\npage {pageCount}\n");
  foreach (var person in page)
  {
    itemCount++;
    System.Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
  }
  System.Console.WriteLine($"\nend page {pageCount}\n");
}
System.Console.WriteLine($"There are {pageCount} pages an {itemCount} documents");
```

#### ! La dernière `page` est vide dans mon exemple :

```bash
# ...
name: 1099-person, address: xxx, pet: puppy
name: 1100-person, address: xxx, pet: puppy
end page 11


Begin page 12
end page 12

There are 12 pages an 1100 documents
```





## `stateless`

Si on a des millions de documents, il est préférable d'avoir une approche `stateless`.

Un `continuation token` est donné avec chaque page, il suffit de l'envoyer avec sa prochaine requête pour demander les documents suivants.

```cs
async static Task QueryWithStatelessPaging()
{
  // Récupérer toutes les page en utilisant le continuation token
  var continuationToken = default(string);

  var pageCount = 0;
  do
  {
    pageCount++;
    System.Console.WriteLine($"Begin page {pageCount}");

    continuationToken = await QueryFetchNextPage(continuationToken);
    System.Console.WriteLine($"continuation token returned: {continuationToken}");

    System.Console.WriteLine($"End page {pageCount}");
  } while (continuationToken != null);

  System.Console.WriteLine("all document retrieved stateless");
}
```

Une boucle `while` appelle les documents jusqu'à ce que le `continuation token` soit `null`.

`default(string)` renvoie la valeur par défaut d'un `string` c'est à dire `null`.

```csharp
async static Task<string> QueryFetchNextPage(string continuationToken)
{
  var container = Client.GetContainer("Users", "Families");
  var sql = "SELECT * FROM c";

  var iterator = container.GetItemQueryIterator<Person>(sql, continuationToken, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });
  var page = await iterator.ReadNextAsync();

  var itemCount = 0;

  if (continuationToken != null)
  {
    Console.WriteLine($"...resuming with continuation {continuationToken}");
  }

  foreach (var person in page)
  {
    itemCount++;
    // Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
  }
  System.Console.WriteLine($"There are {itemCount} documents");
  continuationToken = page.ContinuationToken;

  if (continuationToken == null)
  {
    System.Console.WriteLine("...No more continuation; resultset complete");
  }

  return continuationToken;
}
```

On voi qu'on passe le `continuation token` à `GetItemQueryIterattor` :

#### `c.GetItemQueryIterator<Person>(sql, cToken, rOptions)`

```bash
# ...
End page 10
Begin page 11
...resuming with continuation [{"token":"-RID:~z1EnAKnaSSToAwAAAAAAAA==#RT:10#TRC:1000#ISV:2#IEO:65567#QCF:3","range":{"min":"","max":"FF"}}]
There are 100 documents
continuation token returned: [{"token":"-RID:~z1EnAKnaSSRMBAAAAAAAAA==#RT:11#TRC:1100#ISV:2#IEO:65567#QCF:3","range":{"min":"","max":"FF"}}]
End page 11
Begin page 12
...resuming with continuation [{"token":"-RID:~z1EnAKnaSSRMBAAAAAAAAA==#RT:11#TRC:1100#ISV:2#IEO:65567#QCF:3","range":{"min":"","max":"FF"}}]
There are 0 documents
...No more continuation; resultset complete
continuation token returned: 
End page 12
all document retrieved stateless
```

Il y a une page en plus vide ici aussi.

On peut avoir la page et le nombre de document total dans le `continuation token` : `RT:11#TRC:1100`,

Ici `11` et `11OO`.



## `code` complet

```csharp
using System;
using System.Threading.Tasks;
using Microsoft.Azure.Cosmos;

namespace azure_cosmos_connection
{
    class Program
    {
        static CosmosClient Client { get; set; } = Shared.Client;

        async static Task Main(string[] args)
        {
            // await QueryWithStatefulPaging();
            await QueryWithStatelessPaging();
            // await Create1100Person();
        }

        async static Task Create1100Person()
        {
            var container = Shared.Client.GetContainer("Users", "Families");

            for (int i = 0; i < 1100; i++)
            {
                var p = new Person
                {

                    Id = $"{Guid.NewGuid()}",
                    Name = $"{i + 1}-person",
                    Address = "xxx",
                    MyPet = new Pet
                    {
                        Nickname = "puppy"
                    }
                };
                await container.CreateItemAsync(p);
            }
            System.Console.Clear();
            System.Console.WriteLine("1100 persons was created");
        }

        async static Task QueryWithStatefulPaging()
        {
            System.Console.WriteLine("\n>>> Query Documents (SQL) <<<\n");

            // On récupère le container
            var container = Client.GetContainer("Users", "Families");

            var sql = "SELECT * FROM c";

            var iterator = container.GetItemQueryIterator<Person>(sql, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });
            var page = await iterator.ReadNextAsync();

            System.Console.WriteLine($"There are {page.Count} documents retrieved");
            foreach (var person in page)
            {
                System.Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
            }

            // Get all pages using iterator.HasMoreResults
            iterator = container.GetItemQueryIterator<Person>(sql, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });

            var itemCount = 0;
            var pageCount = 0;

            while (iterator.HasMoreResults)
            {
                pageCount++;
                page = await iterator.ReadNextAsync();
                System.Console.WriteLine($"\nBegin page {pageCount}");
                foreach (var person in page)
                {
                    itemCount++;
                    System.Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
                }
                System.Console.WriteLine($"end page {pageCount}\n");
            }
            System.Console.WriteLine($"There are {pageCount} pages an {itemCount} documents");
        }

        async static Task QueryWithStatelessPaging()
        {
            // Récupérer toutes les page en utilisant le continuation token
            System.Console.WriteLine("Querying for all documents (full result, stateless)");

            var continuationToken = default(string);

            var pageCount = 0;
            do
            {
                pageCount++;

                System.Console.WriteLine($"Begin page {pageCount}");

                continuationToken = await QueryFetchNextPage(continuationToken);
                System.Console.WriteLine($"continuation token returned: {continuationToken}");

                System.Console.WriteLine($"End page {pageCount}");
            } while (continuationToken != null);

            System.Console.WriteLine("all document retrieved stateless");
        }

        async static Task<string> QueryFetchNextPage(string continuationToken)
        {
            var container = Client.GetContainer("Users", "Families");
            var sql = "SELECT * FROM c";

            var iterator = container.GetItemQueryIterator<Person>(sql, continuationToken, requestOptions: new QueryRequestOptions { MaxItemCount = 100 });
            var page = await iterator.ReadNextAsync();

            var itemCount = 0;

            if (continuationToken != null)
            {
                Console.WriteLine($"...resuming with continuation {continuationToken}");
            }

            foreach (var person in page)
            {
                itemCount++;
                // Console.WriteLine($"name: {person.Name}, address: {person.Address}, pet: {person.MyPet.Nickname}");
            }
            System.Console.WriteLine($"There are {itemCount} documents");
            continuationToken = page.ContinuationToken;

            if (continuationToken == null)
            {
                System.Console.WriteLine("...No more continuation; resultset complete");
            }

            return continuationToken;
        }
    }
}
```

