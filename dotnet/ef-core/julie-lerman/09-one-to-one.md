# 09 `On-To-One`



## `Horse`

On ajoute une classe `Horse`, chaque `Samurai` peut avoir un cheval (ou non).

```cs
namespace SamuraiApp.Domain
{
    public class Horse
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public int SamuraiId { get; set; }
    }
}
```

Dans la classe `Samurai` on ajoute un `Horse` :

```cs
using System.Collections.Generic;

namespace SamuraiApp.Domain
{
    public class Samurai
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public List<Quote> Quotes { get; set; } = new();
        public List<Battle> Battles { get; set; } = new();
        public Horse Horse { get; set; }
    }
}
```

On appelle la propriété `Horse`  : `Navigation Property`.

Avec l'`Id` du `Samurai` dans la classe `Horse` et la propriété de navigation `Horse` dans `Samurai`,

`EF Core` a assez d'éléments pour inférer la relation `one-to-one`.

`Navigation Property` : propriété créant une référence vers l'entité associée.

Dans notre cas le `Horse` n'est pas obligatoire pour un `Samurai`.

Par contre un `Horse` a obligatoirement un `Samurai` car un `int` n'est pas `nullable`.



## Observation des `migration`

`Migration_Horse_1`

```cs
columns: table => new
{
    Id = table.Column<int>(type: "int", nullable: false)
        .Annotation("SqlServer:Identity", "1, 1"),
    Name = table.Column<string>(type: "nvarchar(max)", nullable: true),
    SamuraiId = table.Column<int>(type: "int", nullable: false)
},
```

On voit que `SamuraiId` n'est pas `nullable`.

Si on voulait pouvoir créer des `Horse` sans forcement les relier à un `Samurai`, on peut utiliser un `int?` :

```cs
public class Horse
{
    public int Id { get; set; }
    public string Name { get; set; }
    public int? SamuraiId { get; set; }
}
```

`SamuraiId` étant devenue une propriété optionnelle, elle devient `nullable` en base de données :

`Migration_Horse_2`

```cs
migrationBuilder.AlterColumn<int>(
    name: "SamuraiId",
    table: "Horse",
    type: "int",
    nullable: true,
    oldClrType: typeof(int),
    oldType: "int");
```

