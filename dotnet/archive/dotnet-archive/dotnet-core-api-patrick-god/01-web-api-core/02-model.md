# 02 Modèle

On va créer un dossier `Models` et dedans une classe `Character`.

On a besoin de créer un `enum` pour les classes de personnages

`RpgClass.cs`

```cs
namespace dotnet_rpg.Models
{
    public enum RpgClass
    {
        Knight = 1,
        Mage = 2,
        Cleric = 3
    }
}
```
ou
```cs
public enum RpgClass
{
    Knight,
    Mage,
    Cleric
}
```

Et le modèle : `Character.cs`

```cs
namespace dotnet_rpg.Models
{
    public class Character
    {
        public int Id { get; set; }
        public string Name { get; set; } = "Frodo";
        public int HitPoints { get; set; } = 100;
        public int Strength { get; set; } = 10;
        public int Defense { get; set; } = 10;
        public int Intelligence { get; set; } = 10;
        public RpgClass Class { get; set; } = RpgClass.Knight;
    }
}
```



## Syntaxe des propriété avec `=>`

Dans `WeatherForecast` le fichier de démo, il y a une propriété calculée :

```csharp
public int TemperatureC { get; set; }

public int TemperatureF => 32 + (int)(TemperatureC / 0.5556);
```

en fait cette syntaxe est la même que :

```csharp
public int TemperatureC { get; set; }

public int TemperatureF
{
    get
    {
        return 32 + (int)((double)TemperatureC / 0.5556);
    }
}
```

On peut vérifier avec https://sharplab.io/ .

