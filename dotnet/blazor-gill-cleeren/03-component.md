# 03 `Component`

Tout dans `Blazor` est `component`.

Un `component` est contenu dans un fichier `.razor` et utilise la `razor syntax`.

On crée l'application en imbriquant des `component` les un dans les autres.

Les `page` sont des `component` avec la directive `@page`.



## Exemple de `Component`

`CatDisplay.razor`

```asp
<MudText Typo="Typo.h2" Align="Align.Center" Class="pa-8">Pretty cat is displayed</MudText>

<p Class="d-flex gap-3 justify-center align-center">  
    <MudAvatar Size="Size.Large" Color="Color.Secondary" Variant="Variant.Filled">
        <MudIcon 
            Icon="@Icons.Custom.Brands.GitHub" 
            style="color:white"
            Size="Size.Large" />
    </MudAvatar>
    <MudText Typo="Typo.body1">Pussy Name: @PussyName</MudText>
    <MudText Typo="Typo.body2" Color="Color.Error">Pussy Age: @PussyAge</MudText>
</p>

@code {
    public string PussyName { get; set; } = "MiauPolo";
    public int PussyAge { get; set; } = 13;
}
```



## Séparer le `code` du `template` : `partial class`

On peut utiliser les `Partials Class` pour séparer la logique contenu dans `@code { ... }` du template:

`CatDisplay.razor`

```html
<MudText Typo="Typo.h2" Align="Align.Center" Class="pa-8">Pretty cat is displayed</MudText>

<p Class="d-flex gap-3 justify-center align-center">  
    <MudAvatar Size="Size.Large" Color="Color.Secondary" Variant="Variant.Filled">
        <MudIcon 
            Icon="@Icons.Custom.Brands.GitHub" 
            style="color:white"
            Size="Size.Large" />
    </MudAvatar>
    <MudText Typo="Typo.body1">Pussy Name: @PussyName</MudText>
    <MudText Typo="Typo.body2" Color="Color.Error">Pussy Age: @PussyAge</MudText>
</p>
```

`CatDisplay.cs`

```cs
namespace GillCleerenCourse.Components;

public partial class CatDisplay
{
    public string PussyName { get; set; } = "Mow Bill";
    public int PussyAge { get; set; } = 18;
}
```

