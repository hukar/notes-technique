# 04 Le `Hub`

##  Les méthodes du `Hub`

### Retourner une `task`

```cs
public async Task NotifyWatching()
{
    ViewCount++;
    
    await Clients.All.SendAsync("ViewCountUpdate", ViewCount);
}
```

Ici la méthode est `async` et renvoie elle même une `Task`.

On peut aussi simplement retourner la `Task` d'envoie aux `Clients` :

```cs
public Task NotifyWatching()
{
    ViewCount++;
    return Clients.All.SendAsync("ViewCountUpdate", ViewCount);
}
```



### Retourner une valeur

On peut aussi simplement retourner une valeur :

```cs
public string SendSimpleString()
{
    return "alibaba ya baba";
}
```

### `InvokeAsync<Type>`

On invoque la méthode du côté `Client` avec `InvokeAsync<Type>` :

`Component.razor`

```cs
// ...

@code {
    // ...
    private async Task GetSimpleString()
    {
        if(hubConnection is not null)
        {
            simpleString = await hubConnection.InvokeAsync<string>("SendSimpleString");
        }
    }
}
```

Il suffit de passer le nom de la méthode ici `"SendSimpleString"`.



### Retourner une valeur avec paramètres

La méthode peut très bien recevoir des paramètres :

```cs
public string CompleteName(string firstName, string lastName)
    => $"El {firstName} O'{lastName}"; 
```

Dans le `Client` :

```cs
if(hubConnection is not null)
{
    completeName = await hubConnection.InvokeAsync<string>("SendSimpleString", FirstName, LastName);
}
```

