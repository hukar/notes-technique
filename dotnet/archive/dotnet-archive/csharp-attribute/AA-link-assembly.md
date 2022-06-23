# AA Relier deux `assembly`

## Mise en place

On crée un projet et une librairie de classes :

```bash
dotnet new console -n CApp

dotnet new classlib -n LibLinked
```

`LibLink/Class1.cs`

```csharp
using static System.Console;

namespace LibLinked
{
    public class Class1
    {
        public void HyCoco()
        {
            WriteLine("Hey Coco how are you ?");
        }
    }
}
```

`CApp/Program.cs`

```csharp
using System;
using LibLinked;  // ajouter l'import de la librairie

namespace CApp
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
            var x = new Class1();
            x.HyCoco();
        }
    }
}
```

<img src="assets/Screenshot2020-10-23at09.56.53.png" alt="Screenshot 2020-10-23 at 09.56.53" style="zoom:50%;" />

Les deux `assembly` ne se voient pas, il faut ajouter une référence à `CApp`.

```bash
🦄 CApp dotnet add reference ../LibLinked/LibLinked.csproj
Reference `..\LibLinked\LibLinked.csproj` added to the project.

🦄 CApp dotnet run
Hello World!
Hey Coco how are you ?
```
