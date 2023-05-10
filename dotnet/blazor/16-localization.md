# 16 `Localization`



## `CultureInfo`

Si on tente de changer la `culture` avant le démarrage de l'application dans une application `Blazor Wasm`:

`Program.cs`

```cs
// ...
builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });

CultureInfo.DefaultThreadCurrentUICulture = new CultureInfo("id"); // <- ici
CultureInfo.DefaultThreadCurrentCulture = new CultureInfo("id"); // <- ici

await builder.Build().RunAsync();
```

On obtient cette erreur:

![culture-set-before-app-run](assets/culture-set-before-app-run.png)

Pour changer dynamiquement la `culture` pendant le démarrage, il faut ajouter `<BlazorWebAssemblyLoadAllGlobalizationData>`  dans le fichier `.csproj` et le mettre à `true`.

```xml
<PropertyGroup>
  <BlazorWebAssemblyLoadAllGlobalizationData>true</BlazorWebAssemblyLoadAllGlobalizationData>
</PropertyGroup>
```



## Sélection de la `culture`

```html
@page "/"
@using System.Globalization

<p>
    Date: @DateTime.Now.ToLongDateString()<br/>
    Number: @(number.ToString("N2"))
</p>

<p>
    @foreach (var lang in new[] { "fr", "it", "en", "de" })
    {
        <button @onclick="@(() => ChangeLang(lang))">@lang</button>
    }
</p>
```

```cs
@code {
    double number = 4567.98;

    void ChangeLang(string lang)
    {
        CultureInfo.DefaultThreadCurrentCulture = new CultureInfo(lang);
        CultureInfo.DefaultThreadCurrentUICulture = new CultureInfo(lang);
    }
}
```

<img src="assets/selector-of-culture-blazor-wasm.png" alt="selector-of-culture-blazor-wasm" />

## Ce qui fonctionne

- `IStringLocalizer` et `IStringLocalizer<T>`
- `.resx`



## Ce qui ne fonctionne pas

- `IHtmlLocalizer` et `IViewLocalizer`
- `Localizing Data Annotations`

<img src="assets/localization-support-in-blazor.png" alt="localization-support-in-blazor" />



## `Localization` avec `LocalStorage`

### Mise en place

`IStringLocalizer` fait partie du package `Microsoft.Extensions.Localization`. Ce `package` n'est pas par défaut dans `Blazor Wasm`, il faut l'ajouter:

```bash
dotnet add package Microsoft.Extensions.Localization
```

Il faut ensuite l'ajouter au conteneur de `services` dans `Program.cs`:

```cs
builder.Services.AddLocalization();
```

> ## Remarque
>
> Il ne semble pas nécessaire de donner le chemin vers le dossier où se trouve les fichiers de traduction, il ne semble pas non plus que son nom ait une importance (ici je l'appellle `Resources` par simplicité).



### Fichiers `.resx`

Il faut ensuite créer les fichiers de resources `.resx`, un par langue:

<img src="assets/resx-files-crezated-in-a-directory-resource.png" alt="resx-files-crezated-in-a-directory-resource" style="zoom:50%;" />

On obtient un tableau d'affichage de nos traductions ici dans `Rider`:

<img src="assets/editable-file-resx-xml-table.png" alt="editable-file-resx-xml-table" />

> ## Remarque
>
> Il faut créer une `Dummy Class` : `Data.cs` pour le type `T` de `IStringLocalizer<T>`.
>
> Cette classe doit aussi être importé avec un `@using` dans un composant.



### Créer un `selecteur`

```html
<select @bind="Culture">
    @foreach (var c in supportedCultures)
    {
        <option value="@c">@c.DisplayName</option>
    }
</select>
```

```cs
CultureInfo[] supportedCultures = new[]
{
    new CultureInfo("fr"),
    new CultureInfo("en-US"),
    new CultureInfo("it"),
};

public CultureInfo Culture
{
    get => CultureInfo.CurrentCulture;
    set
    {
        if (CultureInfo.CurrentCulture != value)
        {
            CultureInfo.DefaultThreadCurrentCulture = value;
            CultureInfo.DefaultThreadCurrentUICulture = value;
        }
    }
}
```

<img src="assets/problem-with-translation.png" alt="problem-with-translation" style="zoom:67%;" />

Si la date s'affiche correctement (`Globalization`), la traduction de `CodeName` n'est pas chargée (`Localization`) et le composant plus haut ne reçoit pas ces changements.

Pour avoir les traductions **il est obligé de rechargé la page**.

Pour cela on doit stocker la valeur de langue sélectionnée en dehors de l'application : `LocalStorage`.



### `JSInterop` et créer notre `LocalStorage`

On ajoute un script dans `index.html`, s'il était plus grand on pourrait le mettre dans un fichier séparé.

```html
<script>
	window.blazorCulture = {
        get: () => window.LocalStorage['BlazorCulture'],
        set: (value) => window.LocalStorage['BlazorCulture'] = value
    }
</script>
```

On va l'utiliser dans notre composant:

```cs
[Inject]
public NavigationManager NavigationManager { get; set; } = default!;
[Inject]
public IJSRuntime JSRuntime { get; set; } = default!;

// ...


public CultureInfo Culture
{
    get => CultureInfo.CurrentCulture;
    set
    {
        if (CultureInfo.CurrentCulture != value)
        {
            var js = (IJSInProcessRuntime)JSRuntime;
            js.InvokeVoid("blazorCulture.set", value.Name);

            NavigationManager.NavigateTo(NavigationManager.Uri, forceLoad:true);
        }
    }
}
```

Le changement à proprement parlé de la culture se fait maintenant dans `Program.cs` (qui sera rechargée avec `forceload:true`)

```cs
// ...
CultureInfo culture;

var js = app.Services.GetRequiredService<IJSRuntime>();
var result = await js.InvokeAsync<string>("blazorCulture.get");

if (result != null)
{
    culture = new CultureInfo(result);
}
else
{
    culture = new CultureInfo("it");
    await js.InvokeVoidAsync("blazorCulture.set", "it");
}

CultureInfo.DefaultThreadCurrentCulture = culture;
CultureInfo.DefaultThreadCurrentUICulture = culture;

await app.RunAsync();
```

<img src="assets/good-behaviour-for-translation.png" alt="good-behaviour-for-translation" style="zoom:50%;" />

Maintenant tout fonctionne correctement.



## Version avec `Blazored.LocalStorage`

### Mise en place

```bash
dotnet add package Blazored.LocalStorage
```

Et dans `Program.cs`

```cs
using Blazored.LocalStorage;

builder.Services.AddBlazoredLocalStorage(
    config => config.JsonSerializerOptions.ReferenceHandler = ReferenceHandler.IgnoreCycles
);
```

> ## Remarque
>
> À la suite d'une erreur avec un objet `CultureInfo` que j'essayais de placer dans le `LocaleStorage` dans sa version typée, j'obtenais une erreur de type `Circular Reference`.
>
> `ReferenceHandler.IgnoreCycles` assigne une valeur `null` aux propriétés posant problème.
>
> Dans sa version typé, `LocaleStorage` utilise `System.Text.Json` pour `sérialiser` et `désérialiser` les objets.

Ensuite on veut définir la `culture` au démarrage de l'application `Blazor`:

```cs
// toujours dans Program.cs
var localStorage = app.Services.GetRequiredService<ILocalStorageService>();

CultureInfo currentCulture;

var codeLang = await localStorage.GetItemAsStringAsync("codeLang");

if (codeLang is null)
{
    codeLang = "fr"; // la langue par défaut
    await localStorage.SetItemAsStringAsync("codeLang", codeLang);
}

currentCulture = new CultureInfo(codeLang);

CultureInfo.DefaultThreadCurrentCulture = currentCulture;
CultureInfo.DefaultThreadCurrentUICulture = currentCulture;

await app.RunAsync();
```

On utilise `SetItemAsStringAsync` pour éviter la stérilisation de `SetItemAsync`, de même pour `GetItemAsStringAsync`.

### Dans le `composant`

On doit injecter quelques `services`

```ruby
@inject IStringLocalizer<Data> Localizer
@inject ILocalStorageService LocalStorage
@inject NavigationManager NavigationManager
```

On traduit comme ceci:

```ruby
<h1>@Localizer["Title"]</h1>
```

Voici un `selecteur`

```ruby
<select @onchange="OnSelectLang">
    <option disabled>@Localizer["SelectLang"]</option>
    @foreach (var codeLang in _supportedLanguages)
    {
        <option value="@codeLang" selected="@(CultureInfo.CurrentCulture.Name == codeLang)">@codeLang</option>
    }
</select>
```

Je me base ici sur l'événement `@onchange`

```cs
@code {
    readonly string[] _supportedLanguages = new[] { "fr", "it", "en" };

    async Task OnSelectLang(ChangeEventArgs evt)
    {
        await LocalStorage.SetItemAsStringAsync("codeLang", (string)evt.Value!);
        NavigationManager.NavigateTo(NavigationManager.Uri, forceLoad:true);
    }
}
```

On récupère la valeur du `select` avec `evt.Value`, et on force le rechargement de l'application avec `forceload:true`.











