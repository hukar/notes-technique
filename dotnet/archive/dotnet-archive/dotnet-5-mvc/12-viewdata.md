# 12 `ViewData`

- `ViewData` est un dictionnaire faiblement typé qui dérive de la classe `ViewDataDictionnary`.
- `ViewData` est similaire à `ViewBag`.
- Il est utile pour transférer des données du contrôleur à la vue.
- `ViewData` est un dictionnaire contenant des paires clé-valeur où chaque clé doit être un `string`.
- le cycle de vie du `ViewData` est celui de la requête. Il est nettoyé après une redirection.
- la valeur d'une `ViewData` doit être *castée* avant utilisation.



## Utilisation

### Dans le contrôleur

```cs
public class HomeController : Controller
{
  public IActionResult Index()
  {
    ViewData["Header"] = "Studebt Details";
    
    Student student = new () {
      Name= "james",
      Section = "A",
      Gender = "m"
    }
    
    ViewData["Student"] = student;
    
    return View();
  }
}
```



### Dans la vue

```cs
<h1>
  @ViewData["Header"]
</h1>

@{
	var student = ViewBag["Student"] as Student;
}

<p>
  name : @student.Name<br>
  gender : @student.Gender
</p>
```

Il faut *caster* dans la vue la valeur du `ViewData`.



## Différences

- Les deux sont utilisables pour passer des données du contrôleur à la vue.

- `ViewData` est faiblement typé tandis que `ViewBag` utilise les propriétés dynamiques.

  Les deux sont utilisés pour créer des vues faiblements typées.

- `ViewData` utilisent des clé de type `string` alors que `ViewBag` utilise des propriétés dynamiques pour retrouver les données.

- `ViewData` requiert un *cast* pour les types complexes et de vérifier la valeur `null` pour éviter les erreurs.

