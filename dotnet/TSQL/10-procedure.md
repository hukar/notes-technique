# 10 `Stocked Procedure`



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



## `SELECT PROCEDURE`

```sql
CREATE procedure [dbo].[GetContact]
	@Id int
AS
BEGIN
	SELECT [Id]
		  ,[FirstName]
		  ,[LastName]
	  FROM [dbo].[Contacts]
	WHERE Id = @Id;

	SELECT 
		Id,
		ContactId,
		StreetAddress,
		City,
		PostalCode
	FROM [dbo].[Addresses] 
	WHERE ContactID = @Id;

END
```





## `UPDATE PROCEDURE`

Cette `PROCEDURE` va mettre à jour un enregistrement ou créer lui-ci s'il n'existe pas.

```sql
CREATE PROCEDURE [dbo].[SaveContact]
	@Id     	int OUTPUT,
	@FirstName	varchar(50),
	@LastName	varchar(50),	
	@Company	varchar(50),
	@Title		varchar(50),
	@Email		varchar(50)
AS
BEGIN
	UPDATE	Contacts
	SET		FirstName = @FirstName,
			LastName  = @LastName,
			Company   = @Company,
			Title     = @Title,
			Email     = @Email
	WHERE	Id        = @Id

	IF @@ROWCOUNT = 0
	BEGIN
		INSERT INTO [dbo].[Contacts]
           ([FirstName]
           ,[LastName]
           ,[Company]
           ,[Title]
           ,[Email])
		VALUES
           (@FirstName,
           @LastName, 
           @Company,
           @Title,
           @Email);
		SET @Id = CAST(SCOPE_IDENTITY() as int)
	END;
END;
```

`IF @@ROWCOUNT` test le nombre de ligne modifiées.

On remarque le paramètre `OUTPUT` : `@Id int OUTPUT,` que l'on remplie après avec :

```sql
SET @Id = CAST(SCOPE_IDENTITYy() as int)
```



## `DELETE PROCEDURE`

```sql
CREATE PROCEDURE [dbo].[DeleteAddress]
	@Id int
AS
BEGIN
	DELETE FROM Addresses WHERE Id = @Id;
END;
```

