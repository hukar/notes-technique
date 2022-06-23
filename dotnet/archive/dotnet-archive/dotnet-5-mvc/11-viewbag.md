# 11 `ViewBag`

## Dans le contrôleur

On ne va pas utiliser une valeur par défaut pour la `FK` mais proposer une liste de catégories.

Dans l'`action` :

```cs
using Microsoft.AspNetCore.Mvc.Rendering; // SelectItemList

// GET CREATE
public IActionResult Create()
{
  IEnumerable<SelectListItem> CategoryDropDown = 
    _db.ExpenseCategories.Select(i => new SelectListItem
                                 {
                                   Text = i.ExpenseCategoryName,
                                   Value = i.Id.ToString()
                                 });
  ViewBag.CategoryDropDown = CategoryDropDown;
  
  
  return View();
}
```

`ViewBag` est un moyen de passer des données du contrôleur à la vue.

`SelectListSystem` est un type de `MVC` facilitant la configuration de listes déroulantes.

`ViewBag.TypeDropDown` c'est une capacité `dynamic` (nouveauté `c#`)



## Dans la vue

On peut utiliser notre `VIewBag` grâce à `asp-items`.

```html
<label asp-for="ExpenseCategory"></label>

<select asp-for="ExpenseCategoryId" asp-items="@ViewBag.CategoryDropDown" class="form-control">
  <option selected>-- Select Expense Category --</option>
</select>
```



## Notes

Le `select` *automatique* avec `asp-items` créait une erreur si on ne sélectionnait pas une valeur.

Pour utiliser une simple boucle, il n'est pas possible de renvoyer un `IEnumerable` de type anonyme.

J'ai donc dû créer un `DTO` pour avoir un type dans mon `IEnumerable`.

```cs
// ExpenseController
IEnumerable<CategoryDto> CategoryDropDown = _db.ExpenseCategories.Select(i 
  => new CategoryDto
            {
                Text = i.ExpenseCategoryName,
                Value = i.Id.ToString()
            });

ViewBag.Items = CategoryDropDown;
```

 Dans le template on a une boucle `@foreach` :

```html
<select asp-for="ExpenseCategoryId" class="form-control">
  <option value="0" selected>-- Select Expense Category --</option>
  @foreach (var item in ViewBag.Items)
  {
  	<option value="@item.Value">@item.Text</option>
  }
</select>
```

Pour éviter un autre bug j'ai dû ajouter une valeur à la première `option` : `value="0"` et ajouter aussi une validation au modèle :

```cs
[DisplayName("Expense Category")]
[Range(1, int.MaxValue, ErrorMessage = "Id can't be 0 💀")]
public int ExpenseCategoryId { get; set; }
```



## Qu'est-ce qu'un `ViewBag`

- C'est un mécanisme permettant de passer des données d'une `action` d'un `contrôleur` à la `vue` associée.
- `ViewBag` est une propriété `dynamic` de la class `Controller` de base.
- Le passage des données va du contrôleur à la vue, jamais l'inverse : unidirectionnel
- On peut assigner n'importe qu'elle quantité de propriété et de valeur à un `ViewBag`
- La durée de vie du `ViewBag` est celle de la requête `HTTP`, lors d'une redirection, le `ViewBag` prend la valeur `null`.
- `ViewBag` est en réalité une sur-couche de `ViewData`.

### Utilisation

Dans le contrôleur

```cs
public class HomeController : Controller
{
  public IActionResult Index()
  {
    ViewBag.Header = "Studebt Details";
    
    Student student = new () {
      Name= "james",
      Section = "A",
      Gender = "m"
    }
    
    ViewBag.Student = student;
  }
}
```

Dans la vue

```html
<h1>
  @ViewBag.Header
</h1>

@{
	var student = ViewBag.Student;
}

<p>
  name : @student.Name<br>
  gender : @student.Gender
</p>
```

