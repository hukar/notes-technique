# 03 Syntaxe des `Razor Pages`



Chaque `Razor Pages` se compose de deux fichiers :

- `MyPage.cshtml` le template avec la syntaxe `razor`
- `MyPage.cshtml.cs` la logique de la `Page` remplaçant le `controller` de l'architecture `MVC`



## `MyPage.cshtml`

```c#
@page
@model RazorPageMommer.Pages.MyPage
@{
    ViewData["Title"] = "MyPage";
}

<h1>@ViewData["Title"]</h1>

<div>
    
</div>
```

`@quelquechose` c'est la syntaxe `razor`.

Ce code interpréter sera ré-injecté dans `_Layout.cshtml` grâce à `@renderBody()` :

```cs
// ...
</header>
    <div class="container">
        <main role="main" class="pb-3">
            @RenderBody()
        </main>
    </div>
<footer class="border-top footer text-muted">
    // ...
```

