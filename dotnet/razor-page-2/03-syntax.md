# La syntax `Razor`

la syntaxe `razor` commence par `@` : `@quelquechose`

Cette syntaxe dynamise notre `html`.



## Page `razor`

Une page `razor` se compose de deux fichiers `.cshtml` pour le template et `.cshtml.cs` pour la logique associée au template.

### Créer une `razor page` en `CLI`

```bash
dotnet new page -n CliPage -o Pages
```

`CliPage.cshtml`

```html
@page
@model MyApp.Namespace.CliPageModel
@{
}
```

`CliPage.cshtml.cs`

```cs
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;

namespace MyApp.Namespace
{
    public class CliPageModel : PageModel
    {
        public void OnGet()
        {
        }
    }
}
```

On peut aussi créer une page sans `@model`

```bash
dotnet new page -n NoModel -o Pages -np
```

`-np` `--no-pagemodel`

On obtient un seul fichier `NoModel.cshml`

```cs
@page
@{
}

<!-- on peut écrire son html ici -->
```



## `_Layout.cshtml`

C'est le template de base.

Les autres pages sont rendues grâce à la fonction `@RenderBody()` :

```html
<div class="container">
    <main role="main" class="pb-3">
        @RenderBody()
    </main>
</div>
```



## `@page`

Chaque `page` doit contenir cette instruction.

Cette instruction doit précéder toutes les autres :

<img src="assets/define-page-before-everithiong-hrk.png" alt="define-page-before-everithiong-hrk" style="zoom:50%;" /> 



## `@{ ... }`

On peut écrire dans ce type de block du `c#` classique.

```cs
@{
    ViewData["Title"] = "Privacy Policy";
}
```

### `ViewData`

`ViewData` permet de partager des données entre les pages.















