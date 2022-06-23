# 04 Faire du `mapping` avec un `form`

## Création d'une méthode de type `POST` :

```cs
[HttpPost("editor/{id:int}")]
public async Task<IActionResult> ClientEditor(int id, ClientViewModel model)
{
  // save change to database
  var oldClient = await _context.Clients
    .Include(c => c.Address)
    .Where(c => c.Id == id)
    .FirstOrDefaultAsync();

  if (oldClient is not null)
  {
    // update database
    _mapper.Map(model, oldClient); // copy changes
    
    if(await _context.SaveChangesAsync() > 0)
    {
      return RedirectToAction("Index");
    }
  }

  return View();
}
```

Utilisation du `_context` de `EF Core`.

Utilisation de `FirstOrDefaultAsync` => ne pas oublier `await`.

`Include` pour ajouter les champs de l'objet `Address` référencé.

`_mapper.Map(model, oldClient)` copie les champs modifié  de `model` vers `oldClient`.

`_context.SaveChangesAsync()` renvoie le nombre d'entrée sauvées. (Ici une `Task` contenant ce nombre)



## Un lien vers le formulaire dans `Index.cshtml`

```cs
<div class="col-8 offset-2">
  <h3>Clients</h3>
  @foreach (var c in Model)
	{
 		 <!-- ... -->
    <a asp-action="ClientEditor" 
       asp-route-id="@c.Id" 
       class="btn btn-sm btn-primary">Edit</a>
    }
</div>
```



## le formulaire : `ClientEditor.cshtml`

```cs
@{
    ViewData["Title"] = "Client Editor";
}
@model ClientViewModel

<div class="row">
    <div class="col-8 offset-2">
        <h3>Client Editor</h3>
        <form method="post">
            <input type="hidden" asp-for="Id" />
            <div class="form-group">
                <label asp-for="Name"></label>
                <input asp-for="Name" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="ContactName"></label>
                <input asp-for="ContactName" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="Phone"></label>
                <input asp-for="Phone" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="Address1"></label>
                <input asp-for="Address1" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="Address2"></label>
                <input asp-for="Address2" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="Address3"></label>
                <input asp-for="Address3" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="CityTown"></label>
                <input asp-for="CityTown" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="StateProvince"></label>
                <input asp-for="StateProvince" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="PostalCode"></label>
                <input asp-for="PostalCode" class="form-control" />
            </div>
            <div class="form-group">
                <label asp-for="Country"></label>
                <input asp-for="Country" class="form-control" />
            </div>
            <div class="form-group">
                <a asp-action="Index" class="btn btn-info">Cancel</a>
                <input type="submit" value="Save" class="btn btn-primary" />
            </div>
        </form>
    </div>
</div>
```

