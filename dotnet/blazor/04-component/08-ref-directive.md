# 08 `@ref`directive

C'est un moyen de communication du `composant enfant` vers le `composant parent`.

On peut créer une référence sur un composant pour pouvoir l'utiliser directement dans le code :

- lire ses propriétés
- Exécuter ses méthodes

```cs
<MoviesList @ref="moviesList" Movies="movies" />
  
@code {
  private MoviesList moviesList;
  
  moviesList.SuperAction();
}  
```

On déclare une variable du type du composant : les composants sont des `partial class`.

On exécute ensuite une méthode `SuperAction` définie dans le `@code` du composant.



## Exemple : Une `Modal` de confirmation

On crée un composant qui ouvre une boite de confirmation :

`Confirmation.razor`

```cs
@if(displayConfirmation)
{
  <div>
  	@Title
    @ChildContent
    <button @onclick="OnCancel">Cancel</button>
    <button @onclick="OnConfirm">Confirm</button>
  </div>
}

@code {
  private bool displayConfirmation = false;
  
  [Parameter] public string Title { get; set; } = "Confirm";
  [Parameter] public RenderFragment? ChildContent { get; set; }
  [Parameter] public EventCallback OnCancel { get; set; }
  [Parameter] public EventCallback OnConfirm { get; set; }
  
  public void Show() => displayConfirmation = true;
  public void Hide() => displayConfirmation = false;
}
```

Le composant expose deux méthodes `Show` et `Hide` permettant de gérer l'affichage de la `Modal`.

`RenderFragment` permet de passer du contenu **enfant** au composant.

`EventCallback` permet de passer une référence vers une méthode du **parent**.

Dans le composant **parent** :

`Parent.razor`

```cs
<button @onclick="DeleteMovie(movie)">Delete</button>

<!-- ... -->

<Confirmation @ref="confirmation" OnCancel="OnCancel" OnConfirm="OnConfirm">
  <h3>Do you want to Delete this Movie : @movieToDelete.Title ?</h3> 
</confirmation>
    
@code {
    public Confirmation confirmation;
    
    public Movie movieToDelete;
    
    public void OnDelete(Movie movie)
    {
      movieToDelete = movie;
      confirmation.Show();
    }
    
    public void OnCancel()
    {
      confirmation.Hide();
      movieToDelete = null;
    }
    
    public void OnConfirm()
    {
      if(movieToDelete is not null) movies.Remove(movieToDelete);
      confirmation.Hide();
      movieToDelete = null;
    }    
}    
```

Grâce à la `@ref` sur le composant `Confirmation`, on peut accéder à ses méthodes d'affichage. 