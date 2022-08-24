# 10 `SqlCommandBuilder`

`SqlCommandBuilder` génère automatiquement les instructions `INSERT`, `UPDATE` et `DELETE` depuis l'instruction `SELECT` récupérant une seule table.

## Récupérer un seul enregistrement

```cs
var connectionString = builder.Configuration.GetConnectionString("HukarConnectionTwo");
builder.Services.AddScoped(_ => new SqlConnection(connectionString));

app.MapGet("/student/{id:int}", GetStudentById);		
```

POur la suite je prépare un `Dictionary` capable de récupérer le `DataSet`.

```cs
Dictionary<string, DataSet> memo = new();
```



```cs
GetStudentDto? GetStudentById(SqlConnection con, int id)
{
    string sql = "SELECT * FROM Student where Id = @Id";

    SqlDataAdapter dataAdapter = new(sql, con);
    dataAdapter.SelectCommand.Parameters.AddWithValue("@Id", id);

    DataSet dataSet = new();

    dataAdapter.Fill(dataSet, "Students");
    
    memo["DATA_SET"] = dataSet;

    if(dataSet.Tables["Students"]!.Rows.Count == 0)
    {
        return null;
    }

    DataRow row = dataSet.Tables["Students"]!.Rows[0];

    return new GetStudentDto(
        Convert.ToInt32(row["Id"]),
        row["Name"].ToString(),
        row["Gender"].ToString(),
        Convert.ToInt32(row["TotalMarks"])
    );
}
```

On voit un `overload` de `Fill` en donnant le nom à la table chargée.

```cs
dataAdapter.Fill(dataSet, "Students");
```



## `Update` student

### Version avec `SqlCommand`

```cs
ResponseDto UpdateStudent(SqlConnection con,  GetStudentDto updatedStudent)
{
    string sql = "UPDATE Student SET Name = @Name, Gender = @Gender, TotalMarks = @TotalMarks WHERE Id = @Id";

    SqlCommand cmd = new(sql, con);
    cmd.Parameters.AddWithValue("@Name", updatedStudent.Name);
    cmd.Parameters.AddWithValue("@Gender",updatedStudent.Gender);
    cmd.Parameters.AddWithValue("@TotalMarks",updatedStudent.TotalMarks);
    cmd.Parameters.AddWithValue("@Id",updatedStudent.Id);

    con.Open();
    int rowsAffected = cmd.ExecuteNonQuery();

    return new ResponseDto(rowsAffected);
}
```



### Version avec `SqlDataAdapter`

```cs
ResponseDto UpdateStudentSda(SqlConnection con, GetStudentDto updatedStudent)
{
    string sql = "SELECT * FROM Student where Id = @Id";
    SqlDataAdapter dataAdapter = new(sql, con);
    dataAdapter.SelectCommand.Parameters.AddWithValue("@Id", updatedStudent.Id);
    
    SqlCommandBuilder builder = new(dataAdapter);
    DataSet ds = memo["DATA_SET"];
    
    if(ds.Tables["Students"]!.Rows.Count > 0) {
        DataRow row = ds.Tables["Students"]!.Rows[0];
        
        row["Name"] = updatedStudent.Name;
        row["Gender"] = updatedStudent.Gender;
        row["TotalMarks"] = updatedStudent.TotalMarks;
    }
    else {
        return new ResponseDto(0);
    }
    
    int rowsAffected = dataAdapter.Update(ds, "Students");

    return new ResponseDto(rowsAffected);
}
```

Il faut cependant que l'enregistrement ait été appelé au moins une fois, sinon le `DataSet` est vide.

Ou pire on enregistre les données dans le mauvaise `Id`.



























