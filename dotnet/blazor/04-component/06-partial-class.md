

# 06 `Partial Class` 

Les composant `Blazor` sont compilé comme des `Partial Class`.

## Séparation du `code` et du `template`

On peut si on le désire séparer en deux fichiers le `code` et le `template`

### `Code`

`Counter.razor.cs`

```cs
namespace BlazorNETUG.Pages;

public partial class Counter
{
    private int currentCount = 0;

    private void IncrementCount()
    {
        currentCount += 3;
    }
}
```



### `Template`

`Counter.razor`

```html
@page "/counter"
@page "/ici"
@page "/aussi/ici"

<PageTitle>Counter</PageTitle>

<h1>Counter</h1>

<p role="status">Current count: @currentCount</p>

<button class="btn btn-primary" @onclick="IncrementCount">Click me</button>
```

On doit avoir une `partial class` et les noms de fichier doivent correspondres.



## Injection de dépendance : `@inject`

On peut placer l'injection de dépendance du côté du code grâce à l'annotation `[Inject]`:

```cs
using Microsoft.AspNetCore.Components;

public partial class Counter
{
    [Inject]  public SingletonService singleton { get; set; } = new();
    [Inject] public TransientService transient { get; set; } = new();
  // ...
```

Autre exemple avec `?` plutôt que `new()` :

```cs
using System.Net.Http.Json;
using CarRentManagement.Client.Static;
using CarRentManagement.Shared.Domain;
using Microsoft.AspNetCore.Components;
using Microsoft.JSInterop;

namespace CarRentManagement.Client.Pages.Colours;
public partial class Index
{
    [Inject] HttpClient? httpClient { get; set; }
    [Inject] IJSRuntime? jSRuntime { get; set; }

    private List<Colour>? Colours;

    protected async override Task OnInitializedAsync()
    {
        Colours = await httpClient!.GetFromJsonAsync<List<Colour>>(Endpoints.ColoursEndpoint);
    }

    private async Task Delete(int colourId)
    {
        var colour = Colours!.First(q => q.Id == colourId);

        if(await jSRuntime!.InvokeAsync<bool>("confirm", $"Do you want to delete {colourId}"))
        {
            await httpClient!.DeleteAsync($"{Endpoints.ColoursEndpoint}/{colourId}");
            // Refresh the page
            await OnInitializedAsync(); 
        }
    }
}
```

Les `global using` n'ont pas l'air de fonctionner avec `Blazor Wasm`.

