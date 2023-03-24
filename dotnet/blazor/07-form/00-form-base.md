# 00 `Form` de base

Syntaxe d'un composant de formulaire de base :

`ItemForm.razor`

```cs
<EditForm Model="Item" OnValidSubmit="OnvalidSubmit">
  <DataAnnotationsValidator />
  
  <label>Title :</label>     
  <InputText class="form-control" @bind-Value="Movie.Title" />
  <ValidationMessage For="() => Movie.Title" />
  
</EditForm>
  
@code {
  [Parameter] public Item? Item { get; set; }
  [Parameter] public EventCallback OnValidSubmit { get; set; }
}  
```

Deux paramètres permettent de passer le `Model` et la fonction `callback` en cas de `soumission` du formulaire.