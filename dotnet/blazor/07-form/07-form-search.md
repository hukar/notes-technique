# 07 Formulaire de recherche

Exemple de formulaire de recherche:

```csharp
@page "/Movies/Search"
  @inject IRepository repository 

<input id="title" type="text"
  @bind="title" @bind:event="oninput"
    @onkeypress="(KeyboardEventArgs e) => TitleKeyPress(e)">
<select @bind="selectedGenre">
    <option value="0">-- Select genre --</option>
    @foreach (var genre in genres)
    {
      <option value="@genre.Id">@genre.Name</option>
    }
</select>

<input type="checkbox" @bind="upcomingReleases">

<input type="checkbox" @bind="inTheathers">

<button type="button" @onclick="SearchForMovies">Filter</button>
<button type="button" @onclick="Clear">Clear</button>

  // affichage du résultat
<MoviesList Movies="movies" />

  @code {

  List<Movie>? movies;

  string title = "";
  string selectedGenre = "0";
  // données hardcodées
  List<Genre> genres = new() {
    new() { Id = 1, Name = "Comedy"},
    new() { Id = 2, Name = "Action" },
    new() { Id = 3, Name = "SF" },
  };
  bool upcomingReleases;
  bool inTheathers;

  protected override void OnInitialized()
  {
    movies = repository.GetMovies();
  }

  public void SearchForMovies()
  {
    movies = repository.GetMovies()
      .Where(m => m.Title.Contains(title))
      .ToList();
    // ici exemple avec title seulement
  }

  public void Clear()
  {
    movies = repository.GetMovies();

    title = "";
    selectedGenre = "0";
    upcomingReleases = false;
    inTheathers = false;
  }

  public void TitleKeyPress(KeyboardEventArgs e)
  {
    Console.WriteLine($" e.Key : {e.Key}");
    if (e.Key == "Enter")
    {
      SearchForMovies();
    }
  }
}
```

> ## Amélioration de la recherche
>
> `Contains` à un `overload` qui permet de le rendre `case insensitive` :
>
> ```cs
> movies = repository.GetMovies().Where(m => m.Title.Contains(title, StringComparison.CurrentCultureIgnoreCase)).ToList();
> ```
>
> `StringComparison` est un `enum` contenant plusieurs valeurs.
>
> Un article plus détaillé ici :
>
> https://docs.microsoft.com/en-us/dotnet/api/system.stringcomparison?view=net-6.0
