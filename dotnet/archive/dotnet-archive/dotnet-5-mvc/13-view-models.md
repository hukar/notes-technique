# 13 `ViewModels`

- Les `View Models` sont des classes utilisées pour rendre ou lier des vues.

- `Loosely typed View` des vues qui utilisent plusieurs classes comme modèle :

  Un `@model` et un `ViewBag` par exemple

- `Strongly Typed View` une vue qui n'utilise qu'un seul modèle avec `@model`.

- Les `ViewModels` sont très similaire aux `models`.

- Les `ViewModels` sont uniquement utilisés pour rendre une `vue`.

- On met tous les `View Model` dans un dossier `ViewModels`.
- Elle fonctionnent comme intermédiaire entre les classes de la `DB`
- La `vue` est fortement couplée avec les `View Models`, pour les opérations de `DB`, on parse le `ViewModel` en classe `Model` de base.
- Il y a des outils tiers qui automatisent et simplifient le processus : `Auto Mapper`.



## Les `View Models` en action

On va créer un dossier `Models/ViewModels` et dedans une classe `ExpenseVM` :

```cs
namespace InAndOut.Models.ViewModels
{
    public class ExpenseVM
    {
        public Expense Expense { get; set; }
      	public IEnumerable<categoryDto> categoryDropDown { get; set; }
    }
}
```

Cette classe contient tout ce dont la vue a besoin.

On va l'utiliser maintenant dans le contrôleur :

```cs
public IActionResult Create()
{
	var expenseVM = new ExpenseVM()
  {
    Expense = new(),
    CategoryDropDown = _db.ExpenseCategories.Select(i => new categoryDto(){
      Text = i.expenseCategoryName,
      Value = i.Id.ToString()
    })
  }

  return View(expenseVM);
}
```

On va maintenant modifier la vue :

```html
@{
	ViewData["Title"] = "Expense - Create Form";
}
@model InAndOut.Models.ViewModels.ExpenseVM

<form method="post" asp-action="Create">

  <label asp-for="Expense.ExpenseName"></label>

  <label asp-for="Expense.Amount"></label>

  <label asp-for="Expense.ExpenseCategoryId"></label>

  <input asp-for="Expense.ExpenseName" class="form-control" />
  <span class="text-danger" asp-validation-for="Expense.ExpenseName"></span>

  <input asp-for="Expense.Amount" class="form-control" />
  <span class="text-danger" asp-validation-for="Expense.Amount"></span>

  <select asp-for="Expense.ExpenseCategoryId" class="form-control">
    <option value="0" selected>-- Select Expense Category --</option>
    @foreach (var item in Model.CategoryDropDown)
    {
    	<option value="@item.Value">@item.Text</option>
    }
  </select>
  <span class="text-danger" asp-validation-for="Expense.ExpenseCategoryId"></span>

  <input type="submit" class="btn btn-info w-75" value="create" />
  <a asp-action="Index" class="btn btn-danger w-75">Back</a>


  @section Scripts
  {
  	@{ <partial name="_ValidationScriptsPartial" /> }
  }
```

Maintenant on doit modifier l'`action` : `Create Post` car celle-ci reçoit un `ExpenseVM` au lien d'un `Expense` :

```cs
[HttpPost]
[ValidateAntiForgeryToken]
public IActionResult Create(ExpenseVM obj) // <=
{
  if (ModelState.IsValid)
  {
    _db.Add(obj.Expense); // <=
    _db.SaveChanges();

    return RedirectToAction("Index");
  }

  return View(obj);
}
```

