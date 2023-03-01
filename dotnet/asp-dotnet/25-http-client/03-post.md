# 03 `Post` : Créer une ressource



## Utilisation de `HttpRequestMessage` et `SendAsync`

```cs
public async Task CreateResource()
{
  var movieToCreate = new MovieForCreation() {
    Title = "Titi vs Toto",
    Description = "Bla bla bla ...",
    DirectorId = Guid.Parse("d28888e9-2ba9-473a-a40f-e38cb54f9b35"),
    ReleaseDate = new DateTimeOffset(new DateTime(1992, 9, 2)),
    Genre = "Crime, Drama"
  };
  
  var serializedMovieToCreate = JsonSerializer.Serialize(movieToCreate);
  
  using HttpRequestMessage request = new HttpRequestMessage(HttpMethod.Post, "api/movies");
  request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
  
  request.Content = new StringContent(serializedMovieToCreate);
  request.Content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
  
  using HttpResponseMessage response = await _httpClient.SendAsync(request);
  response.EnsureSuccessStatusCode();
  
  var content = await response.Content.ReadAsStringAsync();
  
  var createdMovie = JsonSerializer.Deserialize<Movie>(content,
      new JsonSerializerOptions() {
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
      });
}
```



## Définir Les `Headers` d'une `Request`

Il y a trois endroits où cela set possible :

<img src="assets/headers-setting-three-place-to-define.png" alt="headers-setting-three-place-to-define" style="zoom:50%;" />

`HttpClient.DefaultRequestHeaders` : Pour toutes les requêtes.

`HttpRequestMessage.Headers` : pour une certaine requête.

`HttpRequestMessage.Content.Headers` : `header` concernant le contenu de la requête comme `Content-Type`.

On peut aussi spécifier le `Content-Type` directement dans un `StringContent` :

```cs
 new StringContent(
    JsonSerializer.Serialize(movieToCreate), 
    Encoding.UTF8, 
    "application/json" // <= Content-Type
  )
```





### `HttpRequestMessage.Content` de type `HttpContent`

C'est un type `abstract` duquel dérive des type correspondant au contenu de la requête :

- `StringContent`
- `ObjectContent`
- `ByteArrayContent`
- `StreamContent`
- ...

Ces classes ont des méthodes optimisées pour ces types spécifiques de contenu.

C'est une bonne pratique de les utiliser le plus possible.



## Alternative avec `PostAsync`

```cs
var movieToCreate = new MovieForCreation() { ... };

var response = await _httpClient.PostAsync(
	"api/movies",
  new StringContent(
    JsonSerializer.Serialize(movieToCreate), 
    Encoding.UTF8, 
    "application/json"
  )
);

response.EnsureSuccessStatusCode();
// ...
```









