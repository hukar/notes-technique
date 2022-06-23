# 14 `MudBlazor`

## Ajouter `MudBlazor`

```bash
dotnet add package MudBlazor
```

### `_Import.razor`

```cs
@using MudBlazor
```



### `Program.cs`

```cs
using MudBlazor.Services; // <= ici

// ...
builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });

builder.Services.AddMudServices(); // <= ici

await builder.Build().RunAsync();
```



### `wwwroot/index.html`

On ajoute dans le `head`:

```html
		<base href="/" />
    
    <!-- Mudblazor -->
    <link href="https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap" rel="stylesheet" />
    <link href="_content/MudBlazor/MudBlazor.min.css" rel="stylesheet" />
    <!-- End Mubblazor -->

    <link href="css/app.css" rel="stylesheet" />
    <link href="MySite.styles.css" rel="stylesheet" />
</head>
```

Je le mets avant `app.css` et `MySite.style.css` pour pouvoir écraser les styles de `Mudblazor si besoin.`

`MySite.style.css` étant le fichier de `style` généré grâce aux `css isolation`.

On retire :

- le contenu du fichier `css/app.css`

- Les dossier `iconic` et `bootstrap`

- Le lien dans `index.html`

  ```html
  <link href="css/bootstrap/bootstrap.min.css" rel="stylesheet" />
  ```



On ajoute dans le body le `script` :

```html
		<script src="_framework/blazor.webassembly.js"></script>

    <script src="_content/MudBlazor/MudBlazor.min.js"></script> <!-- ici -->
    <script>navigator.serviceWorker.register('service-worker.js');</script>
</body>
```



### `MainLayout.razor`

On ajoute les `Provider` au `layout`.

Seul `<MudThemeProvider/>` est obligatoire :

```cs
@inherits LayoutComponentBase

<MudThemeProvider/>
<MudDialogProvider/>
<MudSnackbarProvider/>
```



## Ajouter un layout de base

Dans la section `Wireframe` on a plusieurs `layout` qu'il suffit de copier/coller dans son fichier `MainLayout.razor`.

### Super Simple : juste `MudAppBar`

```cs
<MudLayout>
    <MudAppBar Elevation="1">
        <MudText Typo="Typo.h5" Class="ml-3">Application</MudText>
        <MudSpacer />
        <MudIconButton Icon="@Icons.Material.Filled.MoreVert" Color="Color.Inherit" Edge="Edge.End" />
    </MudAppBar>
    <MudMainContent>
        @*Body*@
    </MudMainContent>
</MudLayout>
```



## Utiliser le `Template` de base `MudBlazor`

https://github.com/MudBlazor/Templates

On installe le `template` sur sa machine :

```bash
dotnet new --install MudBlazor.Templates
```

On l'utilise :

```bash
dotnet new mudblazor --host wasm --name MyApplication
```

`-ho` `--host` : `wasm` ,  `wasm-hosted`, `wasm-pwa` ,  `wasm-pwa-hosted`, `server`

défaut : `wasm`

