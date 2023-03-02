# 01 `Blazor` project

## Création du projet en `.net 7` avec `MudBlazor`

```bash
dotnet new blazorwasm-empty
```

Pour ne pas avoir `Bootstrap` et les `Pages` d'exemple.

On ajoute manuellement `Mudblazor`:

```bash
dotnet add package mudblazor
```

Dans `_Imports.razor`

```cs
@using MudBlazor
```

Dans `index.html`

```html
<link href="https://fonts.googleapis.com/css?family=Roboto:300,400,500,700&display=swap" rel="stylesheet" />
<link href="_content/MudBlazor/MudBlazor.min.css" rel="stylesheet" />

<!-- ... -->

	<script src="_framework/blazor.webassembly.js"></script>
    <script src="_content/MudBlazor/MudBlazor.min.js"></script>
</body>
```

Dans `Program.cs`

```cs
using MudBlazor.Services;

builder.Services.AddMudServices();
```

Dans `MainLayout.razor`

```html
<MudThemeProvider/>
<MudDialogProvider/>
<MudSnackbarProvider/>
```



## Choix et ajout du `template`

De base:

```html
<MudLayout>
    <MudMainContent>
        @Body
    </MudMainContent>
</MudLayout>
```

<img src="assets/base-layout-mudblazor.png" alt="base-layout-mudblazor" />



Avec une `AppBar` et un `Drawer`:

```html
<MudLayout>
    <MudAppBar>
        My Application
    </MudAppBar>
    <MudDrawer Open="true">

    </MudDrawer>
    <MudMainContent>
        @Body
    </MudMainContent>
</MudLayout>
```

<img src="assets/template-with-appbar-drawer.png" alt="template-with-appbar-drawer" />

Avec un bouton `hamburger`:

```html
<MudLayout>
    <MudAppBar>
        <MudIconButton Icon="@Icons.Material.Filled.Menu" Color="Color.Inherit" Edge="Edge.Start" OnClick="@((e) => DrawerToggle())" />
        My Application
    </MudAppBar>
    <MudDrawer @bind-Open="@_drawerOpen">
        
    </MudDrawer>
    <MudMainContent>
        @Body
    </MudMainContent>
</MudLayout>
@code {
    bool _drawerOpen = true;

    void DrawerToggle()
    {
        _drawerOpen = !_drawerOpen;
    }
}
```

<img src="assets/open-and-close-drawer.png" alt="open-and-close-drawer" />



## Centrer le contenu `MudContainer`

```html
<MudMainContent>
    <MudContainer MaxWidth="MaxWidth.Medium">
        @Body
    </MudContainer>
</MudMainContent>
```

<img src="assets/mudcontainer-center-content.png" alt="mudcontainer-center-content" />







