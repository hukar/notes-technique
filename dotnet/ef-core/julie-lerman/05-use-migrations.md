# 05 Utiliser les `migrations`

## généré un script `SQL` : `migrations script`

```bash
dotnet ef migrations script -o samuraidb.sql
Build started...
Build succeeded.
```

`samuraidb.sql`

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

CREATE TABLE [Samourais] (
    [Id] int NOT NULL IDENTITY,
    [Name] nvarchar(max) NULL,
    CONSTRAINT [PK_Samourais] PRIMARY KEY ([Id])
);
GO

CREATE TABLE [Quotes] (
    [Id] int NOT NULL IDENTITY,
    [Text] nvarchar(max) NULL,
    [SamouraiId] int NOT NULL,
    CONSTRAINT [PK_Quotes] PRIMARY KEY ([Id]),
    CONSTRAINT [FK_Quotes_Samourais_SamouraiId] FOREIGN KEY ([SamouraiId]) REFERENCES [Samourais] ([Id]) ON DELETE CASCADE
);
GO

INSERT INTO [__EFMigrationsHistory] ([MigrationId], [ProductVersion])
VALUES (N'20210507131636_Init', N'5.0.5');
GO

COMMIT;
GO


```

Avec le script, on est responsable de créer la `BDD` avant l'exécution du `sql`.



## généré directement la `BDD` : `database update`

Si la base de données  n'existe pas encore, elle est générée automatiquement par la `migration`.

```bash
dotnet ef database update --verbose
```

