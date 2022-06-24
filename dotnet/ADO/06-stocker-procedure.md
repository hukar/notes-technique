# 06 `Stocked Procedure`





## Exemple avec OUTPUT Parameter

 Script `sql` de `procédure stockée`

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

Dans le `INSERT` on a des `parameter` en entrée, tandis que pour le `SELECT` on a un `parameter` en sortie (`OUTPUT`).

`SCOPE_IDENTITY()` pour récupérer le dernier `Id` inséré.

Pour définir un paramètre sortant on a la syntaxe `@Id int OUTPUT`.



### Appeler la `procédure stockée`

```cs
public IResult InsertProductOutputParameter()
{
    var sql = "ProductInsert";
    int ProductInsertedId;
    
    try {
        using var cnn = new SqlConnection(cnnString);
        using var cmd = new SqlCommand(sql, cnn);

        cmd.CommandType = CommandType.StoredProcedure; // <= !!!
		
        // INPUT PARAMETER
        cmd.Parameters.Add(new SqlParameter("@Name", "New Bike"));
        cmd.Parameters.Add(new SqlParameter("@Price", 899));
        
        // OUTPUT PARAMETER
        cmd.Parameters.Add(
            new SqlParameter {
                ParameterName = "@Id",
                Value = -1, // ?
                IsNullable = false,
                DbType = DbType.Int32,
                Direction = ParameterDirection.Output
            }
        );
        
        cnn.Open();
            
        var rowsAffected = cmd.ExecuteNonQuery();
        
        ProductInsertedId = (int)cmd.Parameters["@Id"].Value;
        return Ok(new { rowsAffected, ProductInsertedId });
    }
    catch...
}
```

Tous les `OUTPUT Parameters` sont remplis automatiquement.

Ne pas oublié de mettre `CommandType` à `StoredProcedure`.

<img src="assets/api-result-stored-procedure-tni.png" alt="api-result-stored-procedure-tni" style="zoom:50%;" />

