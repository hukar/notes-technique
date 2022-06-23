# 06 Implémentation d'une classe `Enumeration`

Cette classe permet d'avoir à la fois une valeur numérique `Value` et une valeur texte `Name`.



## Version simplifiée de Steve Smith

```cs
// Adaptation personnel simplifiée
public class Role
{
    public static Role Representative => new(0, "Representative");
    public static Role Delegate => new(1, "Delegate");
    public static Role Cooker => new(2, "Cooker");
    
    public string Name { get; init; }
    public int Value { get; init; }

    private Role(int value, string name) => (Value, Name) = (value, name);
    

    public static IEnumerable<Role> List() => new List<Role> {Representative, Delegate, Cooker};
}
```



## La classe `Enumeration`

```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace EProlex.Domain.Common
{
    public abstract class Enumeration : IComparable
    {
        public int Value { get; private set; }
        public string Name { get; private set; }

        protected Enumeration(int value, string name) => (Value, Name) = (value, name);

        public override string ToString() => Name;

        public static IEnumerable<T> GetAll<T>() where T : Enumeration
            => typeof(T).GetFields(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly)
                        .Select(f => f.GetValue(null))
                        .Cast<T>();

        public override bool Equals(object obj)
        {
            if (obj is not Enumeration otherValue)
            {
                return false;
            }

            var typeMatches = GetType().Equals(obj.GetType());
            var valueMatches = Value.Equals(otherValue.Value);

            return typeMatches && valueMatches;
        }

        public override int GetHashCode() => Value.GetHashCode();

        public int CompareTo(object other) => Value.CompareTo(((Enumeration)other).Value);
    }
}
```

`ToString` renvoie la valeur texte `Name`.

`BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly`

On veut les champs `public`, `static` et `non hérités`.

`BindinFlags.Public` et `BindingFlags.Static` **doivent** être présent ensemble.

`if (obj is not Enumeration otherValue)` utilise le `pattern matching` pour initialiser la variable `otherValue`.

On peut lire si `obj` n'est pas de type `Enumeration` **et** déclare `otherValue` de type `Enumeration` exécute le bloc suivant, sinon ne fait rien.

`GetHashCode` : Returns: A 32-bit signed integer hash code.



## Exemple de classe héritant de `Enumeration`

```cs
public class Delay : Enumeration
{
    public static readonly Delay WhitoutDelay = new(0, "without delay");
    public static readonly Delay FiveDays = new(5, "5 days");
    public static readonly Delay ThirtyDays = new(30, "30 days");
    public static readonly Delay FourtyFiveDays = new(45, "45 days");
    public static readonly Delay SixtyDays = new(60, "60 days");

    public Delay(int value, string name) : base(value, name) { }
}
```

Chaque élément est un `static` `readonly`.



## Test 

### `GetAll`

```cs
var delays = Delay.GetAll<Delay>();

foreach (var d in delays)
{
    WriteLine(d);
}
```

```bash
without delay
5 days
30 days
45 days
60 days
```

Le `ToString` renvoie la valeur texte.

### Comparaison

```cs
var delay = Delay.WhitoutDelay;
var delay2 = Delay.FiveDays;
var delay3 = Delay.FiveDays;

WriteLine(delay == delay2);
WriteLine(delay2 == delay3);

WriteLine(delay.Equals(delay2));
WriteLine(delay2.Equals(delay3));
```

```bash
False
True

False
True
```

On voie que les comparaisons se font sur la valeur numérique `Value` :

```cs
var delay = Delay.WhitoutDelay;
var delay2 = Delay.FiveDays;
var delay3 = Delay.ThirtyDays;

WriteLine(delay.Value > delay2.Value);
WriteLine(delay2.Value < delay3.Value);
```

```bash
False
True
```

`CompareTo` :

```cs
var delay = Delay.WhitoutDelay;
var delay2 = Delay.FiveDays;
var delay3 = Delay.FiveDays;
var delay4 = Delay.ThirtyDays;

WriteLine(delay2.CompareTo(delay));
WriteLine(delay2.CompareTo(delay3));
WriteLine(delay2.CompareTo(delay4));
```

```bash
1  // plus grand
0  // égal
-1  // plus petit
```

## Implémentation d'une classe d'`enumeration` par Steve Smith (`Ardalis`)

https://ardalis.com/enum-alternatives-in-c/

```cs
// classe originelle
public class Role {
    public static Role Author {get;} = new Role(0, "Author");
    public static Role Editor {get;} = new Role(1, "Editor");
    public static Role Administrator {get;} = new Role(2, "Administrator");
    public static Role SalesRep {get;} = new Role(3, "Sales Representative");

    public string Name { get; private set; }
    public int Value { get; private set; }

    private Role(int val, string name) 
    {
        Value = val;
        Name = name;
    }

    public static IEnumerable<Role> List()
    {
        // alternately, use a dictionary keyed by value
        return new[]{Author,Editor,Administrator,SalesRep};
    }

    public static Role FromString(string roleString)
    {
        return List().Single(r => String.Equals(r.Name, roleString, StringComparison.OrdinalIgnoreCase));
    }

    public static Role FromValue(int value)
    {
        return List().Single(r => r.Value == value);
    }
}
```



