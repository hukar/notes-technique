# 05 `Stocked Procedure`



## Créer une `Procedure`

```sql
USE AdoTestApi
Go

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
CREATE PROCEDURE ProductInsert
	@Name VARCHAR(50),
	@Price FLOAT,
	@Id int OUTPUT
AS
INSERT INTO Product (Name, Price)
VALUES (@Name, @Price)

SELECT @Id = SCOPE_IDENTITY();
```



## Modifier une `Procedure`

```sql
USE AdoTestApi
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE ProductInsert
	@Name nvarchar(150),
	@IntroductionDate datetime,
	@Price decimal,
	@Id int OUTPUT
AS
INSERT INTO Product (Name, IntroductionDate, Price)
VALUES (@Name, @IntroductionDate, @Price);

SELECT @Id = SCOPE_IDENTITY();
```

