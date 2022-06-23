# 12 `Bulk` operation

`EF Core` estime les bénéfice d'une `Bulk` Operation à partir de `4` enregistrement.

Pour tester cela on vas donc enregistrer `4` samurais :

```bash
info: 10/05/2021 14:16:05.832 RelationalEventId.CommandExecuted[20101] (Microsoft.EntityFrameworkCore.Database.Command) 
      Executed DbCommand (59ms) [Parameters=[@p0='Michel' (Size = 4000), @p1='Micheline' (Size = 4000), @p2='Robi' (Size = 4000), @p3='Tina' (Size = 4000)], CommandType='Text', CommandTimeout='30']
      SET NOCOUNT ON;
      DECLARE @inserted0 TABLE ([Id] int, [_Position] [int]);
      MERGE [Samurais] USING (
      VALUES (@p0, 0),
      (@p1, 1),
      (@p2, 2),
      (@p3, 3)) AS i ([Name], _Position) ON 1=0
      WHEN NOT MATCHED THEN
      INSERT ([Name])
      VALUES (i.[Name])
      OUTPUT INSERTED.[Id], i._Position
      INTO @inserted0;
      
      SELECT [t].[Id] FROM [Samurais] t
      INNER JOIN @inserted0 i ON ([t].[Id] = [i].[Id])
      ORDER BY [i].[_Position];
```

`EF Core` exécute un `MERGE JOIN`.

Cela évite de pratiquer plusieurs `INSERT` à la suite.

Le `SELECT` de fin, permet à `EF Core` de renvoyer les `Id` générés en base de données aux objets de l'application.



## `AddRange`

Pour enregistrer plusieurs objets en même temps il y a la méthode `AddRange`.

```cs
void AddVariousTypes()
{
    _context.Samurais.AddRange(
        new Samurai { Name = "Alf" },
        new Samurai { Name = "Doni" }
    );

    _context.Battles.AddRange(
        new Battle { Name = "Battle of Toilet Area" },
        new Battle { Name = "Battle of Popota desert" }
    );

    _context.SaveChanges();
}
```

Il faut aussi que la totalité des insertion soit au moins égal à `4`, mais ce sont deux `MERGE JOIN` qui sont générés :

```sql
DECLARE @inserted0 TABLE ([Id] int, [_Position] [int]);
MERGE [Battles] USING (
VALUES (@p0, 0),
(@p1, 1)) AS i ([Name], _Position) ON 1=0
WHEN NOT MATCHED THEN
INSERT ([Name])
VALUES (i.[Name])
OUTPUT INSERTED.[Id], i._Position
INTO @inserted0;

SELECT [t].[Id] FROM [Battles] t
INNER JOIN @inserted0 i ON ([t].[Id] = [i].[Id])
ORDER BY [i].[_Position];

DECLARE @inserted2 TABLE ([Id] int, [_Position] [int]);
MERGE [Samurais] USING (
VALUES (@p2, 0),
(@p3, 1)) AS i ([Name], _Position) ON 1=0
WHEN NOT MATCHED THEN
INSERT ([Name])
VALUES (i.[Name])
OUTPUT INSERTED.[Id], i._Position
INTO @inserted2;

SELECT [t].[Id] FROM [Samurais] t
INNER JOIN @inserted2 i ON ([t].[Id] = [i].[Id])
ORDER BY [i].[_Position];
```

On a toujours les `SELECT` pour renvoyer les `Id`.

## `_context.AddRange`

On peut aussi directement utiliser `AddRange` sur le `context`.

Le `SQL` à produire sera inféré des types des objets passés en paramètre :

```cs
void AddVariousTypes()
{
    _context.AddRange(
        new Samurai { Name = "Alf" },
        new Samurai { Name = "Doni" },
        new Battle { Name = "Battle of Toilet Area" },
        new Battle { Name = "Battle of Popota desert" }
    );

    _context.SaveChanges();
}
```

Le même `sql` que précédemment sera généré.

## `MaxBatchSize`

`Batch` : traitement par lot, ici l'enregistrement de plusieurs objets.

On peut modifier la limite du nombre maximum d'enregistrement ( `Batch Size`) dans `optionsBuilder` :

```cs
optionsBuilder.UseSqlServer(connectionString, options => options.MaxBatchSize(150));
```

