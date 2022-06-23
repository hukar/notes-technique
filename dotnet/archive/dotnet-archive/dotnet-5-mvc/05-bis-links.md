# 05-bis Créer un lien vers une `action`

dans le fichier `Views/Shared/_Layout.cshtml` on ajoute un lien au menu :

```html
<a class="nav-link text-dark" asp-area="" asp-controller="Item" asp-action="Index">
  My Borrowed Item
</a>
```

On a `asp-controller` pour spécifier le nom du `controller`.

Et `asp-action` pour spécifier le nom de l'`action`.

<img src="assets/borrowed-item-link-6770767.png" alt="borrowed-item-link" style="zoom:50%;" />

