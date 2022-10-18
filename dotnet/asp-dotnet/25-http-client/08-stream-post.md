# 08 Utilisation des `streams` : `Post`

On peut aussi utiliser les `streams` pour envoyer des données.

De base on a le flow suivant :

<img src="assets/flow-without-stream-post-datas-to-server.png" alt="flow-without-stream-post-datas-to-server" style="zoom:50%;" />

On peut améliorer le processus avec des `streams` :

<img src="assets/streaming-post-sending-datas-to-server.png" alt="streaming-post-sending-datas-to-server" style="zoom:50%;" />

On saute le passage en mémoire de la sérialisation vers `json` et on utilise comme `HttpContent` :`StreamContent` au lieu de `StringContent`.



## Création d'un `POST Stream` : `System.Text.Json`

https://code-maze.com/using-streams-with-httpclient-to-improve-performance-and-memory-usage/

Mise en place :

```cs
public class StreamService : IIntegrationService
{
    private static readonly HttpClient _httpClient = new();
    private readonly JsonSerializerOptions _options;

    public StreamService()
    {
        _httpClient.BaseAddress = new Uri("http://localhost:5000");
        _httpClient.Timeout = TimeSpan.FromSeconds(30);
        _httpClient.DefaultRequestHeaders.Accept();
        _options = new() {
            PropertyNamingPolicy = JsonNamingPolicy.CamelCase
        };
    }
```

Implémentation de la méthode `POST Stream`

```cs
private async Task PostPosterWithStreamMicrosoftJson()
{
    // generated poster
    Random random = new();
    byte[] generatedBytes = new byte[524288];
    random.NextBytes(generatedBytes);

    PosterForCreation posterForCreation = new() {
        Name = "The Big Lebowski poster II",
        Bytes = generatedBytes
    };
    
    using MemoryStream memoryStream = new();
    await JsonSerializer.SerializeAsync(memoryStream, posterForCreation);
    memoryStream.Seek(0, SeekOrigin.Begin);
    
	using HttpRequestMessage request = new(HttpMethod.Post, $"api/movies/bb6a100a-053f-4bf8-b271-60ce3aae6eb5/posters");
    request.Headers.Accept.Add(new MediaTypeWithQualityHeaderValue("application/json"));
    
    using StreamContent requestContent = new(memoryStream);
    request.Content = requestContent;
    requestContent.Headers.ContentType = new MediaTypeHeaderValue("application/json");
    
    using HttpResponseMessage response = await _httpClient.SendAsync(request, HttpCompletionOption.ResponseHeadersRead);
    
    response.EnsureSuccessStatusCode();
    
    using Stream content = await response.Content.ReadAsStreamAsync();
    Poster createdPoster = await JsonSerializer.DeserializeAsync<Poster>(content, _options);
}
```





## Création d'un `Post Stream` : `Newtonsoft.Json`

```cs
private async Task PostPosterWithStream()
{
    // Création de l'enregistrement
    var random = new Random();
    var generatedBytes = new byte[524288];
    random.NextByte(generatedBytes);

    var posterForCreation = new PosterForCreation() {
        Name = "A new poster for The Big Lebowski",
        Bytes = generatedBytes
    };

    // On évite de créer un string en mémoire ici
    // => JsonConvert.SerializeObject(posterForCreation)
    // Création d'un stream en mémoire
    using memoryStream = new MemoryStream();
    using StreamWriter streamWriter = new(
        memoryStream, new UTF8Encoding(), 1024, true
    );

    using JsonTextWriter jsonTextWriter = new(streamWriter);
    JsonSerializer jsonSerializer = new();
    jsonSerializer.Serialize(jsonTextWriter, posterForCreation);
    jsonTextWriter.Flush();

}
```

On créer un `stream` en mémoire avec `MemoryStream`.

On veut ensuite lire dans le `stream` avec `StreamWriter`.

`1024` c'est la taille du `buffer` utilisé par le `stream`.

`true` pour que le `stream` reste ouvert après l'appelle de `Dispose()` car on aura encore besoin de lui pour le *poster*. 

`jsonTextWriter.Flush()` vide le `buffer`.



### Création d'une `Extension Method`

On va directement simplifier (factoriser) le code grâce à une `extension method` :

```cs
// Dans la classe StreamExtensionMethod
public static void SerializeToJsonAndWrite<T>(this Stream stream, T objectToWrite)
{
    if(stream is null)
    {
        throw new ArgumentNullException(nameof(stream));
    }
    if(!stream.CanWrite)
    {
        throw new NotSupportedException("can't write this stream");
    }

    using StreamWriter streamWriter = new(stream, new UTF8Encoding(), 1024, true);
    using newtonsoft.JsonTextWriter jsonTextWriter = new(streamWriter);
    newtonsoft.JsonSerializer jsonSerializer = new();
    jsonSerializer.Serialize(jsonTextWriter, objectToWrite);
    jsonTextWriter.Flush();
}
```



### De retour dans notre méthode `PostPosterWithStream`

```cs
private async Task PostPosterWithStream()
{
    // Création de l'enregistrement
    Random random = new();
    byte[] generatedBytes = new byte[524288];
    random.NextBytes(generatedBytes);

    PosterForCreation posterForCreation = new() {
        Name = "A new poster for The Big Lebowski",
        Bytes = generatedBytes
    };

    // Création d'un stream en mémoire
    MemoryStream memoryStream = new();
    memoryStream.SerializeToJsonAndWrite<PosterForCreation>(posterForCreation);

    // mettre le stream en position 0
    memoryStream.Seek(0, SeekOrigin.Begin);

    // Pour appeller Dispose automatiquement on utilise using
    using HttpRequestMessage request = new(
        HttpMethod.Post,
        $"api/movies/bb6a100a-053f-4bf8-b271-60ce3aae6eb5/posters"
    );

    request.Headers.Accept.Add(
        new MediaTypeWithQualityHeaderValue("application/json"));
    using StreamContent streamContent = new(memoryStream);

    streamContent.Headers.ContentType = new MediaTypeHeaderValue("application/json");
    request.Content = streamContent;

    using HttpResponseMessage response = await _httpClient.SendAsync(request);
    response.EnsureSuccessStatusCode();

    string createdContent = await response.Content.ReadAsStringAsync();
    Poster createdPoster = JsonConvert.DeserializeObject<Poster>(createdContent);
}
```

