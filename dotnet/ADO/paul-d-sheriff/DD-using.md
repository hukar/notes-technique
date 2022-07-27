# DD `Using`

## Bonne pratique

Il faut toujours utiliser `using` avec une classe implémentant `IDisposable`.

On ne sait jamais si l'implémentation a changée, donc `toujours` utiliser `using` même si cela peut sembler inutile ou redondant.

Par exemple `SqlConnection`, `SqlCommand` et `SqlDataReader`.



### règle sur `stackoverflow`

https://stackoverflow.com/questions/2655978/is-closing-disposing-an-sqldatareader-needed-if-you-are-already-closing-the-sqlc

> À mon avis, il y a deux règles à suivre ici :
>
> 1. Les classes qui implémentent `IDisposable` doivent être enveloppées dans un bloc `using`.
> 2. Vous ne devez pas compter sur l'implémentation d'`IDisposable` par une classe pour ignorer la règle 1.





## `Dispose` pour `SQLCommand`

https://stackoverflow.com/questions/16985876/sqlconnection-sqlcommand-sqldatareader-idisposable

### Implémentation

```cs
protected override void Dispose(bool disposing)
{
    if (disposing)
    {
        this._cachedMetaData = null;
    }
    base.Dispose(disposing);
}
```

Pour `SqlCommand`, le `_cacheMetaData` est mit à `null` pour pouvoir être nettoyé.



## `using` pur `SqlConnection`

`Dispose` va appeler `Close` et donc libérer une connection du `Pool`.



## `using statement` et `using declaration` : `nested` form

Test avec https://sharplab.io

### `using statement`

```cs
public class C {
    public void M() {
        using(var test = new TestUsing())
        {
             using(var test2 = new TestUsing())
             {
                 Console.WriteLine(test.ToString());
             }
        }
    }
}

public class TestUsing : IDisposable
{
    public void Dispose()
    {
        Console.WriteLine("hello dispose");
    }
}
```

Compile vers :

```cs
public void M()
{
    TestUsing testUsing = new TestUsing();
    try
    {
        TestUsing testUsing2 = new TestUsing();
        try
        {
            Console.WriteLine(testUsing.ToString());
        }
        finally
        {
            if (testUsing2 != null)
            {
                ((IDisposable)testUsing2).Dispose();
            }
        }
    }
    finally
    {
        if (testUsing != null)
        {
            ((IDisposable)testUsing).Dispose();
        }
    }
}
```



### `using declaration`

```cs
public void M() {
    using var test = new TestUsing();
    using var test2 = new TestUsing();

    Console.WriteLine(test.ToString());
}
```

compile vers :

```cs
public void M()
{
    TestUsing testUsing = new TestUsing();
    try
    {
        TestUsing testUsing2 = new TestUsing();
        try
        {
            Console.WriteLine(testUsing.ToString());
        }
        finally
        {
            if (testUsing2 != null)
            {
                ((IDisposable)testUsing2).Dispose();
            }
        }
    }
    finally
    {
        if (testUsing != null)
        {
            ((IDisposable)testUsing).Dispose();
        }
    }
}
```

C'est excatement la même chose.