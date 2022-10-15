# 05 `DELETE`



## Implémentation

```cs
public async Task DeleteResource()
{
    using HttpRequestMessage request = new(HttpMethod.Delete, "/api/movies/6e87f657-f2c1-4d90-9b37-cbe43cc6adb9");
    request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));

    using HttpResponseMessage response = await _httpClient.SendAsync(request);

    response.EnsureSuccessStatusCode();

    var content = response.Content.ReadAsStringAsync();
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

