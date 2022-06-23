# 03 `Migration`

## `Package`

Pour réaliser les `migrations` on doit installer un nouveau `package` :

```bash
dotnet add package Microsoft.EntityFrameworkCore.design
```



## Commandes pour les `migrations`

```bash
dotnet ef migrations add FirstMigration
```

```bash
dotnet ef database update
```

Pour retirer la dernière `migration` :

```bash
dotnet ef migrations remove
```

Pour obtenir le script `SQL`

```bash
dotnet ef migrations script
```

```sql
IF OBJECT_ID(N'[__EFMigrationsHistory]') IS NULL
BEGIN
    CREATE TABLE [__EFMigrationsHistory] (
        [MigrationId] nvarchar(150) NOT NULL,
        [ProductVersion] nvarchar(32) NOT NULL,
        CONSTRAINT [PK___EFMigrationsHistory] PRIMARY KEY ([MigrationId])
    );
END;
GO

BEGIN TRANSACTION;
GO

CREATE TABLE [Dishes] (
    [Id] int NOT NULL IDENTITY,
    [Title] nvarchar(100) NULL,
    [Notes] nvarchar(100) NULL,
    [Stars] int NULL,
    CONSTRAINT [PK_Dishes] PRIMARY KEY ([Id])
);
GO

CREATE TABLE [Ingredients] (
    [Id] int NOT NULL IDENTITY,
    [Description] nvarchar(100) NOT NULL,
    [UnitOfMeasure] nvarchar(50) NOT NULL,
    [Amount] decimal(5,2) NOT NULL,
    [DishId] int NOT NULL,
    CONSTRAINT [PK_Ingredients] PRIMARY KEY ([Id]),
    CONSTRAINT [FK_Ingredients_Dishes_DishId] FOREIGN KEY ([DishId]) REFERENCES [Dishes] ([Id]) ON DELETE CASCADE
);
GO

CREATE INDEX [IX_Ingredients_DishId] ON [Ingredients] ([DishId]);
GO

INSERT INTO [__EFMigrationsHistory] ([MigrationId], [ProductVersion])
VALUES (N'20220217132822_FirstMigration', N'6.0.2');
GO

COMMIT;
GO

BEGIN TRANSACTION;
GO

ALTER TABLE [Dishes] ADD [Observation] nvarchar(max) NOT NULL DEFAULT N'';
GO

INSERT INTO [__EFMigrationsHistory] ([MigrationId], [ProductVersion])
VALUES (N'20220217134401_SecondMigration', N'6.0.2');
GO

COMMIT;
GO
```

