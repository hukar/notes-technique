# 03. Création d'une page de `Login`



## Mise en place

On créé un dossier `Account` dans le dossier `Pages` et dedans deux fichier :

`Login.cshtml` et `Login.cshtml.cs`

```html
@page
@model SecurityFranckLiu.Pages.Account.Login
@{
    ViewData["Title"] = "Login";
}

<h1>@ViewData["Title"]</h1>

<form method="post">
    <div class="text-danger" asp-validation-summary="ModelOnly"></div>
    <label asp-for="Credential.UserName"></label>
    <input class="form-control" type="text" asp-for="Credential.UserName" />
	<span class="text-danger" asp-validation-for="Credentila.UserName"></span>
    
    <label asp-for="Credential.Password"></label>
    <input class="form-control" type="text" asp-for="Credential.Password" />
    <span class="text-danger" asp-validation-for="Credentila.Password"></span>
    
    <input type="submit" class="btn btn-primary" value="login" />
</form>
```

```cs
public class Login : PageModel
{
    [BindProperty]
    public Credential Credential { get; set; }
    
    public void OnGet()
    {
    }
}

public class Credential
{
    [Required]
    public string UserName { get; set; }
    [Required] 
    [DataType(DataType.Password)]
    public string Password { get; set; }
}
```

