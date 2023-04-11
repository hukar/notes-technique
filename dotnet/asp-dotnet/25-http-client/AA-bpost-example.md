# AA Exemple d'utilisation avec l'`API` de `BPost`

## Version avec `PostAsync`

Voici la requête que l'on veut exécuter depuis `.net`:

```http
POST https://sen-ac2.bpost.cloud/registeredmail/announce HTTP/1.1
x-api-key: toto
```
```json
{
  "accountID": 843249,
  "subaccountID": 573116,
  "routerID": 122598,
  "itemDetails": {
    "barcode": "JJBEA655795432848652048",
    "sender": {
      "name": "Mr. John Doe",
      "companyName": "KBC Bank",
      "department": "Claims dept",
      "place": "CentreMonnaie",
      "street": "Avenue Eugene Plasky",
      "houseNumber": "158",
      "boxNumber": "4",
      "postalCode": "1030",
      "city": "Bruxelles",
      "region": "Bruxelles-Capitale",
      "countryCode": "BE",
      "addressLine1": "Avenue Eugene Plasky 158 bte 4",
      "addressLine2": "1030 Bruxelles Région de Bruxelles-Capitale"
    },
    "addressee": {
      "name": "Mr. John Doe",
      "companyName": "KBC Bank",
      "department": "Claims dept",
      "place": "CentreMonnaie",
      "street": "Avenue Eugene Plasky",
      "houseNumber": "158",
      "boxNumber": "4",
      "postalCode": "1030",
      "city": "Bruxelles",
      "region": "Bruxelles-Capitale",
      "countryCode": "BE",
      "addressLine1": "Avenue Eugene Plasky 158 bte 4",
      "addressLine2": "1030 Bruxelles Région de Bruxelles-Capitale"
    },
    "options": {
      "acknowledgementReceipt": true,
      "anonymousSender": true,
      "rp": true
    },
    "customerReferences": {
      "ref1": "Cust.123654987",
      "ref2": "File.13651874/15618",
      "ref3": "70043170045"
    }
  }
}
```

Et la réponse attendue:

```http
HTTP/1.1 400 Bad Request
Date: Thu, 06 Apr 2023 15:40:25 GMT
# ...Divers Headers
```
```json 
{
  "timestamp": "06-04-2023 05:40:25",
  "status": "error",
  "data": {
    "code": "ACC-08",
    "message": "Mismatch Router ID & main/subAccount ID: Your are not allowed to announce for this account."
  }
}
```



### Code `c#`

```cs
using System;
using System.Net.Http;
using System.Threading.Tasks;

Console.WriteLine("Hello, BPost Http!");

var client = new HttpClient();

// Ajouter des en-têtes personnalisés
client.DefaultRequestHeaders.Add("x-api-key", "toto");

// Utiliser une chaîne de litéraux brut pour inclure le JSON formaté
var json = """
{
 //...
}
""";

var content = new StringContent(json, System.Text.Encoding.UTF8, "application/json");

// Envoyer la requête POST et récupérer la réponse sous forme de chaîne JSON
var response = await client.PostAsync("https://sen-ac2.bpost.cloud/registeredmail/announce", content);
var responseContent = await response.Content.ReadAsStringAsync();

// Afficher la réponse telle quelle
Console.WriteLine(responseContent);
```

C'est dans `StringContent` que l'on définit le `Content-Type`: `application/json`.



### Version avec `SendAsync`

```cs
using System;
using System.Net.Http;
using System.Threading.Tasks;

var json = """
{
  // ..
}
""";

var client = new HttpClient();

HttpRequestMessage request = new();

request.Content = new StringContent(json);
request.Method = HttpMethod.Post;
request.RequestUri = new Uri("https://sen-ac2.bpost.cloud/registeredmail/announce");
request.Content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
request.Headers.TryAddWithoutValidation("x-api-key", "toto");

var response = await client.SendAsync(request);
var responseContent = await response.Content.ReadAsStringAsync();

Console.WriteLine(responseContent);
```

> Par rapport à l'`API` de `BPost` il est obligatoire de renseigner le `Content-Type` à `application/json` dans les `Headers`.
>
> On peut le faire comme ceci:
>
> ```cs
> request.Content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
> ```
>
> ou comme cela:
>
> ```cs
> var content = new StringContent(json, System.Text.Encoding.UTF8, "application/json");
> ```

