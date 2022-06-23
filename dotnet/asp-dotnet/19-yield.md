# 19. `yield`

Avec ce mot clé `yield`on peut créer une séquence de données retournant un `IEnumerable`. (`IEnumerable = Sequence`et non `Collection`)

C'est avec `yield` que l'on peut créer un `Streaming`.

## *`Iterator`* pattern

On va utiliser le type *`IEnumerable<T>`* et le mot clé *`yield`* pour construire un *`Stream`*.

```cs
var numbers = GetNumbers();

foreach(var number in numbers)
{
    Console.WriteLine(number);
}

public IEnumerable<int> GetNumbers()
{
    yield return 1;
    yield return 2;
    yield return 3;
    yield return 4;
}
```

```
1
2
3
4
```

*`yield`* permet de renvoyer les valeurs une par une, C'est très différent que de renvoyer une liste entière de valeurs.

En fait *`var numbers = GetNumbers()`* ne va chercher que la première valeur, puis les autres valeurs dans le *`foreach`*.



## Exécution du code

Il faut faire attention avec *`Iterator Pattern`* au nombre d'appels au procédure de code.

```cs
Console.WriteLine("Assign numbers");
var numbers = GetNumbers();

Console.WriteLine("Foreach Loop");
foreach(var number in numbers)
{
    Console.WriteLine(number);
}

Console.WriteLine("Foreach Loop");
foreach(var number in numbers)
{
    Console.WriteLine(number);
}

IEnumerable<int> GetNumbers()
{
    Console.WriteLine("GetNumbers is called");
    yield return NumberGenerator();
    yield return NumberGenerator();
    yield return NumberGenerator();
    yield return NumberGenerator();
}

int NumberGenerator()
{
    Console.WriteLine("Number Generator is called");
    var randNumber = new Random();
     
     return randNumber.Next(1, 10);
}
```

```
Assign numbers
Foreach Loop
GetNumbers is called
Number Generator is called
7
Number Generator is called
2
Number Generator is called
1
Number Generator is called
6
Foreach Loop
GetNumbers is called
Number Generator is called
8
Number Generator is called
4
Number Generator is called
8
Number Generator is called
7
```

On voit ici que le code *`NumberGenerator`* est appelé huit fois, même si la première ligne de *`GetNumbers`*n'est elle appelée que deux fois.

En fait un pointeur dans le code de la méthode est placé après chaque *`yield return`*, cela peut être gourmand en ressource notament dans le cas d'un appel en base de données.



## `ToList`

On peut résoudre ce problème en utilisant *`ToList`* (comme souvent avec *`EF Core`*).

```cs
var numbers = GetNumbers().ToList();

Console.WriteLine("Foreach Loop");
foreach(var number in numbers)
{
    Console.WriteLine(number);
}

Console.WriteLine("Foreach Loop");
foreach(var number in numbers)
{
    Console.WriteLine(number);
}
```

```
GetNumbers is called
Number Generator is called
Number Generator is called
Number Generator is called
Number Generator is called
Foreach Loop
4
2
3
1
Foreach Loop
4
2
3
1
```

ici le *`ToList`* exécute toutes les itérations et transmet une *`List<int>`* à *`numbers`*.

*`NumberGenerator`*n'est exécuté que 4 fois ce qui est le minimum nécessaire.

On comprend mieux l'utilisation de *`ToList`* lorsqu'on utilise *`EF Core`*.





## Traitement d'un `flux` 

Exemple qui montre comment l'utilisation de `yield` peut améliorer les performances quand il s'agit seulement de renvoyer des données.

`Program.cs`

```cs
var db = new MoviesRepository();

var movies = db.GetAllMovies();

var moviesDto = MapMovies(movies);

foreach(var movieDto in moviesDto)
{
    Console.WriteLine($"{movieDto.Code} - {movieDto.Title} - {movieDto.Now}");
}

IEnumerable<MovieDto> MapMovies(List<Movie> movies)
{
    var moviesDto = new List<MovieDto>();
    
    foreach(var movie in movies)
    {
        var movieDto = new MovieDto() {
            Code = movie.Code,
            Title = movie.Title
        };

        moviesDto.Add(movieDto);
    }

    return moviesDto;
}
```

```
3 DTO is instanciated
31178-93 - X-Men - 11/25/2021 11:48:24
39954-64 - Spider-Man 2 - 11/25/2021 11:48:24
481452-78 - Hulk - 11/25/2021 11:48:24
```

Un cas classique de `mapping` de données reçu d'une `BDD` par exemple.

Si je reçoit `10 000` entrées, je crée `10 000` instance de `MovieDto`.

Si j'essaye de n'avoir qu'une instance :

```cs
IEnumerable<MovieDto> MapMovies(List<Movie> movies)
{
    var moviesDto = new List<MovieDto>();
    var movieDto = new MovieDto();

    foreach (var movie in movies)
    {
        movieDto.Code = movie.Code;
        movieDto.Title = movie.Title;

        moviesDto.Add(movieDto);
    }

    return moviesDto;
}
```

Tous les éléments ont la même `référence` et donc au final la même valeur :

```bash
DTO is instanciated
3 887097-86 - Hulk - 11/25/2021 11:52:53

```

On a par contre qu'un seul passage par le constructeur de `MovieDto`.



## La magie de `yield`

```cs
IEnumerable<MovieDto> MapMovies(List<Movie> movies)
{
    var moviesDto = new List<MovieDto>();
    var movieDto = new MovieDto();

    foreach (var movie in movies)
    {
        movieDto.Code = movie.Code;
        movieDto.Title = movie.Title;

        yield return movieDto;
    }
}
```

Il a suffit de modifier légèrement `MapMovies` avec `yield` pour avoir un seul constructeur :

```bash
DTO is instanciated
645448-84 - X-Men - 11/25/2021 11:55:33
31760-24 - Spider-Man 2 - 11/25/2021 11:55:33
76178-88 - Hulk - 11/25/2021 11:55:33
```

Et aussi chaque film séparé !

Sur `10 000` enregistrement, on économide `9 999` instanciation de `MovieDto`.