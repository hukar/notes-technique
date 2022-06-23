# 12. Exemple avec un `attrribute` : `Wrap`

Créer un `attribute` qui permet de _wrapper_ une `Property` avec une chaîne de caractères.

`Robot.cs`

```csharp
namespace exo
{
    public class Robot
    {
        [Wrap]  // avec le paramètre par défaut ici "*"
        [Wrap("--")] // ajout d'un paramètre
        [Wrap("°°°°°°°°")]
        public string RefName { get; set; }
        public string Company { get; set; }
    }
}
```

`WrapAttribute.cs`

```csharp
using System;

namespace exo
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true)]
    public class WrapAttribute : Attribute
    {
        public string WrapString { get; }
        public WrapAttribute(string wrapString = "*")
        {
            WrapString = wrapString;
        }
    }
}
```

Utilise `System`,

hérite de `Attribute`,

utilise l'`attribute` : `AttributeUsage`

pour définir la cible de l'`attribute` : `Attribute.Targets.Property`

ainsi que s'il peut être multiple : `AllowMultiple = true`.

`Program.cs`

```csharp
using System;
using System.Reflection;
using static System.Console;

namespace exo
{
    class Program
    {
        static void Main(string[] args)
        {
            var r_beartwo = new Robot {
                RefName = "bear two gold08",
                Company = "Magic Technologie"
            };

            DisplayRobot(r_beartwo);
        }

        static void DisplayRobot(Robot r)
        {
            PropertyInfo refNameProperty = typeof(Robot).GetProperty(nameof(Robot.RefName));

            WrapAttribute[] wrapAttributes = (WrapAttribute[])Attribute.GetCustomAttributes(refNameProperty, typeof(WrapAttribute));

            string refName = r.RefName;

            foreach(var w in wrapAttributes)
            {
                refName = $"{w.WrapString}{refName}{w.WrapString}";
            }

            WriteLine(refName);
            WriteLine(r.Company);
        }
    }
}
```

On utilise la `reflection` pour récupérer un `PropertyInfo`.

```cs
using System.Reflection

PropertyInfo myProperty = Type.GetProperty(propertyName);
```

On récupère ensuite le tableau d'`attributes` avec `GetCustomAttributes`

```csharp
CustomAttribute[] customAttrubutes = (CustomAttribute[])Attribute.GetCustomAttributes(nameProperty, typeof(CustomAttribute));
```

<img src="assets/Screenshot2020-10-24at11.16.20.png" alt="Screenshot 2020-10-24 at 11.16.20" style="zoom:33%;" />
