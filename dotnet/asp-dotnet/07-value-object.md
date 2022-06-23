# 07 `Value Object`

## Candidat

- `Name`
- `Email`
- `Address`
- `Color`
- Tout ce qui est mesurable :
  -  `10 Euros` => `Currency`
  - `30° C` => `Temperature`
  - `62 Km` => `Distance`



La règle est tout ce qui n'a pas une identité propre est un bon candidat.

Si toutes les propriétés sont les mêmes, alors c'est la même chose.

Une `citation`, une `norme` peuvent aussi être de bon candidat.

On peut aussi s'en servir pour catégoriser (préciser) une donnée.

- `ClienID Id` plutôt que `Guid id`

- `BirthdayDate BirthdayDate`  plutôt que `DateTime BirthdayDate`



## Caractéristiques

- Pas d'identité (pas d'`Id`)

- Il est immutable (on retire les `setter`), si on veut une autre valeur on instancie un nouvel objet.

- Un `Value Object` doit toujours être valide : on renforce les règles (sinon une `Exception` est levée)

- égalité des valeurs

  > Deux entités sont égales si leur références sont égales.
  > Deux `Value Object` sont égaux si toutes leurs propriétés (significatives) sont égales



Un `Value Object` n'est pas responsable de sa validation mais est responsable de son intégrité.

## Implémentation

### Exemple avec une classe `Norm`

```cs
class Norm : ValueObject
{
    public string Text { get; private set; }
    public string Code { get; private set; }
    // meta donnée qui n'entre pas en compte de la comparaison
    public Guid LastUser { get; private set; }

    public Norm(string text, string code, Guid lastUser)
    {
        Text = text;
        Code = code;
        LastUser = lastUser;
    }
}
```

Pour l'exemple une méta-donnée du dernier utilisateur a été ajouté, cette propriété ne fait pas partie de l'égalité.

> ### `yield`
>
> Cette opérateur permet de construire des `Enumerable` avec une méthode :
>
> ```cs
> foreach (var w in ListOfWeapon())
> {
>     WriteLine(w);
> }
> 
> IEnumerable<string> ListOfWeapon()
> {
>     yield return "Morning Star";
>     yield return "Black Sword";
>     yield return "Bone Arrow";
>     yield return "Blood Axe";
> }
> ```
>
> ```bash
> Morning Star
> Black Sword
> Bone Arrow
> Blood Axe
> ```

### `IEnumerable<object> GetEqualityComponents`

La première chose est de déterminer les champs devant faire partie de l'égalité par valeur :

```csharp
protected abstract IEnumerable<object> GetEqualityComponents;
```

Cela force la classe enfant à implémenter explicitement quels champs elle désire utiliser pour évaluer l'égalité.

Dans notre classe `Norm` il suffit d'implémenter cette méthode :

```cs
protected override IEnumerable<object> GetEqualityComponents()
{
    yield return Text;
    yield return Code;
}
```

On n'utilise `LastUser` qui fait partie des méta-donnée.

### `override Equals`

```cs
public override bool Equals(object obj)
{
    if (obj == null || obj.GetType() != GetType())
    {
        return false;
    }
    
    var other = (ValueObject)obj;
    
    return GetEqualityComponents().SequenceEqual(other.GetEqualityComponents());
}
```


> ### `GetType`
> `GetType` renvoie l'exact `type` au `runtime` de l'instance courante.
> ```cs
> var toto = new Toto();
> WriteLine(toto.GetType());
> 
> class Toto
> {
>     public Toto()
>     {
>         WriteLine(GetType());
>     }
> }
> ```
>
> ```bash
> Toto
> Toto
> ```

> ### `SequenceEqual`
>
> Compare le contenu de deux `IEnumerable` et renvoie `true` s'ils sont identiques.
>
> ```cs
> var list1 = new List<int> { 1, 2, 3 };
> var list2 = new List<int> { 1, 2, 3, 4 };
> var list3 = new List<int> { 1, 2, 3 };
> 
> WriteLine(list1.SequenceEqual(list2));
> WriteLine(list1.SequenceEqual(list3));
> ```
>
> ```bash
> False
> True
> ```



### `override GetHashCode`

```cs
public override int GetHashCode => GetEqualityComponents()
    .Select(x => x != null ? x.GetHashCode() : 0)
    .Aggregate((x,y) => x ^ y);
```

`Aggregate` retourne un `int`. utilise le résultat comme entrée `x` au prochain tour.

```cs
var list = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

var listAlter1 = list.Aggregate((x, y) =>
{
    WriteLine($"x:{x} ^ y:{y} = {x ^ y}");

    return x ^ y;
});
```

```bash
x:1 ^ y:2 = 3
x:3 ^ y:3 = 0
x:0 ^ y:4 = 4
x:4 ^ y:5 = 1
x:1 ^ y:6 = 7
x:7 ^ y:7 = 0
x:0 ^ y:8 = 8
x:8 ^ y:9 = 1
```



## Surcharge (`overload`) des opérateur `==` et `!=`

On utilise les mots clés `public` et `static`.

Les opérateurs `==` et `!=` doivent être surchargés ensembles.

```cs
public static bool operator ==(ValueObject left, ValueObject right)
{
    if(ReferenceEquals(left, null) ^ReferenceEquals(right, null))
    {
        return false;
    }
    
    return ReferenceEquals(left, null) || left.Equals(right);
}

public static bool operator !=(ValueObject left, ValueObject right)
{
    return !(left == right);
}
```

> ### `ReferenceEquals`
>
> renvoie un `bool`, compare les références des objets :
>
> ```cs
> var norm2 = new Norm("the text two of the norm two", "no02", Guid.NewGuid());
> var norm3 = new Norm("the text one of the norm one", "no01", Guid.NewGuid());
> Norm norm4 = null;
> var norm5 = norm3;
> 
> WriteLine(ReferenceEquals(norm2, norm3)); // false
> WriteLine(ReferenceEquals(norm2, null)); // false
> WriteLine(ReferenceEquals(norm4, null)); // true
> WriteLine(ReferenceEquals(norm5, norm3)); // true
> ```


> ### `^` : `XOR` `OU` exclusif 
>
> La table de vérité équivaut au `!=`
>
> ```cs
> WriteLine(true ^ true); // false
> WriteLine(false ^ false); // false
> WriteLine(false ^ true); // true
> WriteLine(true ^ false); // true
> ```

```cs
WriteLine(norm4 == norm5); // true
WriteLine(norm1 == norm2); // false
WriteLine(norm1 == norm3); // true
WriteLine(norm1 == norm4); // false
```



## Classe `ValueObject` entière

```cs
public abstract class ValueObject
{
    protected abstract IEnumerable<object> GetEqualityComponents();

    public override bool Equals(object obj)
    {
        if (obj == null || GetType() != obj.GetType())
        {
            return false;
        }

        var other = (ValueObject)obj;

        return GetEqualityComponents().SequenceEqual(other.GetEqualityComponents());
    }

    public override int GetHashCode() => GetEqualityComponents()
        .Select(x => x != null ? x.GetHashCode() : 0)
        .Aggregate((x, y) => x ^ y);

    public static bool operator ==(ValueObject left, ValueObject right)
    {
        if (ReferenceEquals(left, null) ^ ReferenceEquals(right, null))
        {
            return false;
        }

        return ReferenceEquals(left, null) || left.Equals(right);
    }

    public static bool operator !=(ValueObject left, ValueObject right)
    {
        return !(left == right);
    }
}
```

### `Norm : ValueObject`

```cs
class Norm : ValueObject
{
    public string Text { get; private set; }
    public string Code { get; private set; }

    // meta donnée qui n'entre pas en compte de la comparaison
    public Guid LastUser { get; private set; }

    public Norm(string text, string code, Guid lastUser)
    {
        Text = text;
        Code = code;
        LastUser = lastUser;
    }

    protected override IEnumerable<object> GetEqualityComponents()
    {
        yield return Text;
        yield return Code;
    }
}
```



## `ValueObject` `Email`

