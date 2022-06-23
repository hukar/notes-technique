# `SignalR` Introduction



## `Hub`

Le `Hub` est pour `SignalR` ce qu'est le `Controller` pour une application `MVC` ou `Web API`.

C'est un point de connection de l'application.

C'est un canal bi-directionnel.

On peut bien sûr avoir plusieurs `Hubs`.

<img src="assets/first-schema-hub-bidirectionnel.png" alt="first-schema-hub-bidirectionnel" style="zoom: 33%;" />



# Création d'un `Hub`



## Côté `Server` : `Web` (`Minimal API`)

```bash
dotnet new web -o WebServer
```



Dans une application `Web` on crée un dossier `Hubs` et dedans notre premier `Hub` :

```cs
using Microsoft.AspNetCore.SignalR;
namespace WebServer.Hubs;

public class ViewHub : Hub
{
    public static int ViewCount { get; set; }
    
    public async Task NotifyWatching()
    {
        ViewCount++;
        
        // notify EVERY ONE about new view count
        await Clients.All.SendAsync("ViewCountUpdate", ViewCount);
    }
    
    public async Task SendMessage(string user, string message)
    {
        await Clients.All.SendAsync("ReceiveMessage", user, message);
    }
}
```

Avec `SignalR` toute les méthodes doivent être `async` par défaut.

On signale l'envoie au `Client` en fournissant le nom d'un **événement** :

### `Client.SendAsync("EventName", payload1, payload2)`

### Ajouter le `Service` : dans `Program.cs`

On va mapper le `Hub` à une `URL`, ici `/viewhub` :

```cs
builder.Services.AddSignalR();

// et

app.MapHub<ViewHub>("/viewhub");
```



### Attention aux `Cors`

Il va falloir aussi prendre en charge le problème des `Cors` en ajoutant ceci dans Program.cs :

```cs
builder.Services.AddCors( options => {
    options.AddDefaultPolicy(o => 
        o.WithOrigins("https://localhost:7219", "http://localhost:5151")
        .AllowAnyHeader()
        .AllowAnyMethod()
    );
});

// ...

app.UseCors();
```



## Côté `Client` : `Blazorwasm`

```bash
dotnet new blazorwasm -o ClientPart
```

```bash
cd ClientPart
dotnet add package Microsoft.AspNetCore.SignalR.Client
```



### Mise en place : `HubConnectionBuilder`

```cs
@page "/"
@using Microsoft.AspNetCore.SignalR.Client
@implements IAsyncDisposable
    
    // ...

@code {
    private HubConnection? hubConnection;
    
    // ...
    
    protected override void OnInitialized()
    {  
        hubConnection = new HubConnectionBuilder()
            .WithUrl("http://localhost:5042/chathub")
            .Build();
```



### S'abonner aux événements : `hubConnection.On<T, P>`

```cs
hubConnection.On<string, string>("ReceiveMessage", (user, message) =>
        {
            var encodedMsg = $"{user}: {message}";
            messages.Add(encodedMsg);
            StateHasChanged();
        });
```

On doit préciser les types de ce qui sera reçu : `On<string, string>`.

<img src="assets/type-precision-receive-datas.png" alt="type-precision-receive-datas" style="zoom:50%;" />

En premier argument on a le nom de l'événement `"ReceiveMessage"`.

En deuxième une `Lambda` récupérant les valeurs envoyées avec l'événement.

Pour rafraîchir l'affichage il faut utiliser `StateHasChanged`.



### Démarer la connexion : `hubConnection.StartAsync`

```cs
<button @onclick="StartConnection">Connect You</button>

private async Task StartConnection()
{        
    if(hubConnection is not null && !IsConnected)
    {
        await hubConnection.StartAsync();
    }
    else
    {
        Console.WriteLine("The connection is already open");
    }
}
```

Code pour `IsConnected` : `hubConnection.State`

```cs
public bool IsConnected =>
        hubConnection?.State == HubConnectionState.Connected;
```





### Envoyer un message au `Hub` : `hubConnection.SendAsync`

```cs
private async Task Send()
{
    if (hubConnection is not null)
    {
        await hubConnection.SendAsync("SendMessage", userInput, messageInput);
    }
}
```

Le premier argument `"SendMessage"` est le nom d'une `Method` du `Hub` (côté serveur).

### `hub.SendAsync("HubMethodName", payload1, payload2)`

Ensuite on passe les données pour les paramètres de cette méthode :

```cs
// Côté serveur pour rappel
public async Task SendMessage(string user, string message)
{
    await Clients.All.SendAsync("ReceiveMessage", user, message);
}
```

<img src="assets/correspondance-blazor-server-signalr.png" alt="correspondance-blazor-server-signalr" style="zoom:50%;" />



### Disposer proprement de `Signalr` : `DisposeAsync`

Au début du composant on avait :

```cs
@implements IAsyncDisposable
```

On a donc à la fin du `Blazor Component` ce code :

```cs
@code{
    // ...
    
    public async ValueTask DisposeAsync()
    {
        if (hubConnection is not null)
        {
            await hubConnection.DisposeAsync();
        }
    }
}
```



### Résumé

<img src="assets/summary-transaction-signalr.png" alt="summary-transaction-signalr" style="zoom:50%;" />

```cs
// On importe SignalR
@using Microsoft.AspNetCore.SignalR.Client
    
// on implémente IAsyncDisposable
@implements IAsyncDisposable   

// On déclare notre connexion
private HubConnection? hubConnection;

// On initialise la connexion
hubConnection = new HubConnectionBuilder()
    .WithUrl("http://localhost:5042/chathub")
    .Build();

// On s'abonne à un événement serveur <int> étant le type de count
hubConnection.On<int>("ViewCountUpdate", (count) => {
    counter = count;
    // on met à jour l'affichage sinon cela ne fonctionne pas (ou avec des bugs)
    StateHasChanged();
});

// On démarre la connexion
await hubConnection.StartAsync();

// On envoie un message au serveur, "NotifyWatching" étant une méthode du Hub
await hubConnection.SendAsync("NotifyWatching", userInput, messageInput);

// On ferme proprement la connexion
await hubConnection.DisposeAsync();
```

<img src="assets/synchronocity-signalr-in-da-browser.png" alt="synchronocity-signalr-in-da-browser" style="zoom:50%;" />

