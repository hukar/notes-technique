# 04 `PUT` mettre à jour une ressource

`PUT` met à jour l'entiéreté de la donnée (on passe un objet complet).

`PATCH` met à jour une partie seulement par exemple juste la description.



## `Update` une ressource

```cs
public async Task UpdateResource()
{
  var movieToUpdate = new MovieForUpdate() {
    Title = "Titi vs Toto vs Tata",
    Description = "Bla bla bla ...",
    DirectorId = Guid.Parse("d28888e9-2ba9-473a-a40f-e38cb54f9b35"),
    ReleaseDate = new DateTimeOffset(new DateTime(1992, 9, 2)),
    Genre = "Crime, Drama"
  };
  
  var serializedMovieToUpdate = JsonSerializer.Serialize(movieToUpdate);
  
  var request = new HttpRequestMessage(HttpMethod.Put, "api/movies/91dda11e-fe89-4f96-a427-855d4d18b1e7");
  request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
  request.Content = new StringContent(serializedMovieToUpdate);
  request.Content.Headers.ContentType = new MediaTypeHeaderValue("application/json");
  
  var response = await _httpClient.SendAsync(request);
  response.EnsureSuccessStatusCode();
  
  var content = await response.Content.ReadAsStringAsync();
  
  var updatedMovie = JsonSerializer.Deserialize<Movie>(content, 
      new JsonSerializerOptions {
         PropertyNamingPolicy = JsonNamingPolicy.CamelCase                                                 
      });
}
```



## Alternative avec `PutAsync`

```cs
var movieToUpdate = new MovieForUpdate() { ... };

var response = _httpClient.PutAsync(
	"api/movies/91dda11e-fe89-4f96-a427-855d4d18b1e7",
  new StringContent(
  	JsonSerializer.Serialize(movieToUpdate),
    Encoding.UTF8,
    "application/json"
  )
);

response.EnsureSuccessStatusCode();
// ...
```













