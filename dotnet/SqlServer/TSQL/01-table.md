# 01 `TABLE`

## Créer une `TABLE`

```sql
CREATE TABLE [dbo].[States] (
    [Id]        INT          NOT NULL,
    [StateName] VARCHAR (50) NOT NULL,
    CONSTRAINT [PK_States] PRIMARY KEY CLUSTERED ([Id] ASC)
);
```

Avec `FOREIGN KEY`

```sql
CREATE TABLE [dbo].[Addresses] (
    [Id]            INT          IDENTITY (1, 1) NOT NULL,
    [ContactId]     INT          NOT NULL,
    [AddressType]   VARCHAR (10) NOT NULL,
    [StreetAddress] VARCHAR (50) NOT NULL,
    [City]          VARCHAR (50) NOT NULL,
    [StateId]       INT          NOT NULL,
    [PostalCode]    VARCHAR (20) NOT NULL,
    CONSTRAINT [PK_Addresses] PRIMARY KEY CLUSTERED ([Id] ASC),
    CONSTRAINT [FK_Addresses_Contacts] FOREIGN KEY ([ContactId]) REFERENCES [dbo].[Contacts] ([Id]) ON DELETE CASCADE,
    CONSTRAINT [FK_Addresses_States] FOREIGN KEY ([StateId]) REFERENCES [dbo].[States] ([Id])
);
```



## Renommer une table

`sql server` ne dispose pas de commande pour cela mais d'une procédure stockée :

```sql
EXEC sp_rename 'ancienNom', 'nouveauNom'
```

attention à la virgule.

Ou juste

```sql
sp_rename 'ancienNom', 'nouveauNom'
```





## Ajouter une colonne

```sql
ALTER TABLE Product 
ADD RetireDate DATETIME;
GO
```



## Modifier le type d'une colonne

Pour éviter une erreur de type 

```
ALTER TABLE ALTER COLUMN Price failed because one or more objects access this column.
```

On doit d'abord supprimer la `CONSTRAINT`.

```sql
ALTER TABLE Product
DROP CONSTRAINT DF__Product__Price__267ABA7A;
GO

ALTER TABLE Product
ALTER COLUMN Price decimal
```



## Renomer une colonne

```sql
EXEC sp_RENAME 'table_name.old_name', 'new_name', 'COLUMN'
```

Exemple :

```sql
sp_rename 'Orders.Orders.OredrIsExpedited', 'OrderIsExpedited', 'COLUMN'
```

Ici le nom du `Schema` `Orders` était obligatoire.

## Supprimer une colonne

```sql
ALTER TABLE Contacts
DROP COLUMN IsNew
```



## Modifier la valeur par défaut d'une colonne

```sql
ALTER TABLE Product ADD CONSTRAINT DF_IsDeletedDefault DEFAULT 1 FOR IsDeleted
```

Sans utiliser de contrainte :

```sql
ALTER TABLE Product ADD DEFAULT 1 FOR IsDeleted
```



## Créer une `Foreign Key`

On crée d'abord la colonne :

```sql
ALTER TABLE Product
ADD CategoryId int;
GO
```

Puis on ajoute une contrainte :

```sql
ALTER TABLE Product
ADD CONSTRIANT FK_Product_Category FOREIGN KEY CategoryId
REFERENCES Catgory (Id)
[ON DELETE CASCADE]
[ON UPDATE CASCADE]
```

### Créer une colonne qui est une `FOREIGN KEY`

```sql
ALTER TABLE Product
ADD CategoryId INT
CONSTRAINT FK_Product_Category FOREIGN KEY (CategoryId)
REFERENCES Category (Id)
```

