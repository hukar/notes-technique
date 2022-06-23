# 02 Les basiques de `Linq`



## `IEnumerable` generator

```cs
IEnumerable<int> GenerateNumbers(int maximum)
{
    var random = new Random();

    foreach(var i in Enumerable.Range(1, 10))
    {
        yield return random.Next(maximum);
    }
}
```

Avec le mot clé `yield`, la donnée n'est envoyé que lorsqu'elle est utilisée (consommée).

```cs
var randomListOfNumbers = GenerateNumbers(12);

foreach(var nb in randomListOfNumbers)
{
    Console.WriteLine(nb);
}
```

Dans l'exemple ci-dessus l'appelle réel à `GenerateNumbers`ne s'effectue que dans la boucle `Foreach`, pas avant lors de l'appelle de la première lign (`!`).

C'est un système `Pull Based`, c'est seulement lorsque la boucle appelle `MoveNext` que la fonction `generator` est réellement appelée.

L'opposé en `Push Based` est le `pattern` `Observer`.

`IEnumerable` est `Pull Based`.

Un `Generator` ne renvoie jamais toutes les valeurs d'un seul coup, il les renvoie une par une.



## Exécution déférée

L'ensemble du code n'est pas éxucuté avant d'arriver au `foreach` :

```cs
var randomListOfNumbers = GenerateNumbers(12)
                            .Where(nb => nb % 2 == 0)
                            .Select(nb => (nb * 5) - 1);

// C'est lorsque le in est appelé que le code du generator 
// et des méthodes Linq est exécuté pas avant
foreach(var nb in randomListOfNumbers)
{
    Console.WriteLine(nb);
}
```

On peut décomposer sa `query` en plusieurs étape :

```cs
var results = GenerateNumbers(12);
results = results.Where(nb => nb % 2 == 0);
results = results.Select(nb => (nb * 5) - 1);


foreach(var nb in results)
{
    Console.WriteLine(nb);
}
```

Cela permet de créer une structure logique (`algorithmique`) autour de la query :

```cs
var even = true;

var results = GenerateNumbers(12);

if(even == true)
{
    results = results.Where(nb => nb % 2 == 0);
}
results = results.OrderByDescending(n => n);
results = results.Select(nb => (nb * 5) - 1);


foreach(var nb in results)
{
    Console.WriteLine(nb);
}
```

En sachant que la `Query` finale ne sera exécutée (`exécution déférée`) que dans le `foreach` (ou avec la méthode `Count`).



## `Where` : `Func<T, bool>`

C'est un filtre (un prédicat).

Seul les éléments renvoyant `true` à la condition, sont renvoyé.

```cs
var randomListOfNumbers = GenerateNumbers(12).Where(nb => nb % 2 == 0);

foreach(var nb in randomListOfNumbers)
{
    Console.WriteLine(nb);
}
```

```
8
4
10
4
```



## `Select`

C'est une `projection`, pour chaque valeur on associe une nouvelle valeur à renvoyer.

```cs
var randomListOfNumbers = GenerateNumbers(12)
                            .Where(nb => nb % 2 == 0)
                            .Select(nb => (nb * 5) - 1);
```

```
-1
29
39
39
9
29
```

On peut changer le type de la `List` renvoyée :

```cs
var bonhommesOne = Bonhomme.GetBonhommes(); // IEnumerable<Bonhomme>?
var bonhommesTwo = Bonhomme.GetBonhommes()
                    .Select(b => b.Name); // IEnumerable<string>?
```



## `Count`

Renvoie le nombre d'éléments.

```cs
var results = GenerateNumbers(12);

if(even == true)
{
    results = results.Where(nb => nb % 2 == 0);
}
results = results.Select(nb => (nb * 5) - 1);

Console.WriteLine(results.Count());
```

```
6
```

`Count` tout comme un `Foreach` provoqie l'exécution du `pipeline` d'opérateurs.



## `OrderByDescending`

Ordonne les résultats :

```cs
var results = GenerateNumbers(12);

results = results.OrderByDescending(n => n);
results = results.Select(nb => (nb * 5) - 1);
```

```cs
54
49
49
34
34
24
19
9
```

