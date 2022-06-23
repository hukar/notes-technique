# 05 `DELETE`



## Implémentation

```cs
public async Task DeleteResource()
{
  var request = new HttpRequestMessage(HttpMethod.Delete, "api/movies/3d2880ae-5ba6-417c-845d-f4ebfd4bcac7");
  request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
  
  var response = await _httpClient.SendAsync(request);
  response.EnsureSuccessStatusCode();
  
  var content = await response.Content.ReadAsStringAsync();
}
```

Renvoie normalement un `StatusCode` : `204` `NoContent`.

Si cela fonctionne `content` reçoit une chaîne vide.



## Alternative avec `DeleteAsync`

```cs
var response = _httpClient.DeleteAsync("api/movies/...-845d-f4ebfd4bcac7");
response.EnsureSuccessStatusCode();
// ...
```

