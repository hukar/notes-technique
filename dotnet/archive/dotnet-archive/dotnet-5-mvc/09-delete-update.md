# 09 Effacer une entrée

## Ajout d'un bouton `delete` et un bouton `update`

Dans `Index.cshtml` on va ajouter les boutons :

```html
@foreach (var expense in Model)
{
<tr>
    <td width="30%">@expense.ExpenseName</td>
    <td width="30%">@expense.Amount</td>
    <td width="30%">
        <div class="col-6 text-right">
            <a asp-action="Update" asp-route-Id="@expense.Id" class="btn btn-info w-75">Update</a>
            <a asp-action="Delete" asp-route-Id="@expense.Id" class="btn btn-danger w-75">Delete</a>
        </div>
    </td>
</tr>
}
```

Pour passer l'`Id` de chaque `expense` on utilise :

#### `asp-route-Id="@expense.Id"`.

Cela permet d'avoir des `URL` de type :

```
https://localhost:5001/Expense/Update/12
```



## `Delete`

`Delete.cshtml`

```html
@{
ViewData["Title"] = "Expense - Delete Form";
}
@model InAndOut.Models.Expense

<form method="post" asp-action="DeletePost">
  <input hidden asp-for="Id">
  <!-- ... -->

  <input asp-for="ExpenseName" class="form-control" value="@Model.ExpenseName" disabled />

  <input asp-for="Amount" class="form-control" value="@Model.Amount" disabled />

  <input type="submit" class="btn btn-danger w-75" value="delete" />

  <a asp-action="Index" class="btn btn-info w-75">Back</a>
</form>
```

`<input hidden asp-for="Id">` permet  de transmettre l'`Id` à l'`URL` :

`/delete/{id}`

Les champs sont `disabled`.

Pour ne pas avoir de conflit de nom, on a deux `actions` `Delete` et `DeletePost`.

Et maintenant dans le contrôleur `ExpenseController.cs` :

```cs
public IActionResult Delete(int? id)
{
  if (id == null || id == 0)
  {
    return NotFound();
  }

  var expense = _db.Expenses.Find(id);

  if (expense == null)
  {
    return NotFound();
  }

  return View(expense);
}

[HttpPost]
public IActionResult DeletePost(int? id)
{
  var expense = _db.Expenses.Find(id);

  if (id == null)
  {
    return NotFound();
  }

  _db.Remove(expense);
  _db.SaveChanges();

  return RedirectToAction("Index");
}
```

La première `action` se charge de passer l'`Id` à la vue.

La deuxième supprime réellement l'enregistrement :

```cs
// EF Core
// 1. Tracker l'enregistrement
var exepense = _db.Expenses.Find(id)
  
// 2. Le tagger comme delete
_db.Remove(expense);

// 3. Le supprimer réellement
_db.SavecChanges();
```

> #### simplification de syntaxe
>
> ! Si l'action a le même nom que la vue, on peut ne pas spécifier l'`action` dans le formulaire :
>
> ```html
> // Delete.cshtml
> <form method="post" action="Delete">
> <!-- => -->
> <form method="post">
> ```
>
> 





## `Update`

```cs
public IActionResult Update(int? id)
{
  if (id == null || id == 0)
  {
    return NotFound();
  }

  var expense = _db.Expenses.Find(id);

  if (expense == null)
  {
    return NotFound();
  }

  return View(expense);
}

[HttpPost]
public IActionResult Update(Expense expenseUpdated)
{
  if (ModelState.IsValid)
  {
    _db.Update(expenseUpdated);

    _db.SaveChanges();
    return RedirectToAction("Index");
  }

  return View(expenseUpdated);
}
```

Ici les `actions` peuvent avoir le même nom car elles n'ont pas la même signature.

`_db.Update` modifie toutes les propriétés sans exception, on peut vouloir modifier de manière plus précise :

```cs
var expense = _db.Expenses.Find(expenseUpdated.Id);

expense.ExpenseName = expenseUpdated.ExpenseName;
expense.Amount = expenseUpdated.Amount;

_db.SaveChanges();
```

L'`Id` est passée automatiquement (?) par le formulaire de la vue.

Pas besoin d'ajouter :

```html
<input hidden asp-for="Id">
```

`Update.cshtml`

```html
@{
	ViewData["Title"] = "Expense - Update Form";
}
@model InAndOut.Models.Expense

<form method="post" asp-action="Update">
  @* <input hidden asp-for="Id"> *@

  <input asp-for="ExpenseName" class="form-control" value="@Model.ExpenseName" />
  <span class="text-danger" asp-validation-for="ExpenseName"></span>

  <input asp-for="Amount" class="form-control" value="@Model.Amount" />
  <span class="text-danger" asp-validation-for="Amount"></span>

  <input type="submit" class="btn btn-info w-75" value="update" />

  <a asp-action="Index" class="btn btn-danger w-75">Back</a>

</form>

@section Scripts
{
@{
<partial name="_ValidationScriptsPartial" /> }
}
```

