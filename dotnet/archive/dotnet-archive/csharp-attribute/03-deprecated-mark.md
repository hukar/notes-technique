# 03 Marquer le code comme déprécié : `deprecated`

## `[Obsolete]`

Il faut ajouter `using System`.

```csharp
using System; // On a besoin de System
using static System.Console;

namespace csharp_attributes
{
    public class ContactConsoleWriter
    {
        // ...
        [Obsolete]
        public void Write()
        {
            WriteFirstName();

            WriteAge();
        }

       // ...
    }
}
```

Cela va générer un `warning` au `build` et un surlignage dans `VSCode`.

```bash
dotnet build
```

<img src="assets/Screenshot2020-10-22at14.20.28.png" alt="Screenshot 2020-10-22 at 14.20.28" style="zoom:50%;" />

<img src="assets/Screenshot2020-10-22at14.21.10.png" alt="Screenshot 2020-10-22 at 14.21.10" style="zoom:50%;" />

## `[Obsolete(message)]`

On peut ajouter un message :

```csharp
[Obsolete("My custom message :); thi method will be removed in few times")]
public void Write()
{
    WriteFirstName();

    WriteAge();
}
```

<img src="assets/Screenshot2020-10-22at14.28.12.png" alt="Screenshot 2020-10-22 at 14.28.12" style="zoom:50%;" />

## `[Obsolete(message,error)]`

`error` est un booléen par défaut à `false`.

Si on le passe à `true`, une `error` sera générée à la place du `warning`.

```csharp
[Obsolete("My custom message :); thi method will be removed in few times", true)]
public void Write()
{
    WriteFirstName();

    WriteAge();
}
```

<img src="assets/Screenshot2020-10-22at14.30.51.png" alt="Screenshot 2020-10-22 at 14.30.51" style="zoom:50%;" />

> #### Afficher les `overloads` : `shift` + `command` + `space`

<img src="assets/Screenshot2020-10-22at14.32.23.png" alt="Screenshot 2020-10-22 at 14.32.23" style="zoom:50%;" />
