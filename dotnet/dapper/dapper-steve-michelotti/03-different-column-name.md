# 03 Noms différents entre la classe et la `DB`

On peut parfois avoir un nom de propriété qui ne *match* pas avec le nom de la colonne en `DB` :

`Contact`

```cs
public class Contact
{
    public int Id { get; set; }
    public string FName { get; set; } // <= ici
    public string LastName { get; set; }
    // ...
```

`FName` ne matche pas avec le nom de colonne `FirstName` en `DB`.

Résultat :

```json

[
  {
    "id": 1,
    "fName": null,
    "lastName": "Jordan",
    "email": "michael@bulls.com",
    "company": "Chicago Bulls",
    "title": "MVP",
    "isNew": false,
    "addresses": []
  },
```

le `"fname"` revient `null` de la `DB`



## Solution : créer un alias `sql`

Modifions la requête du `repository` :

```cs
var sql = "SELECT FirstName FName,* FROM Contacts";

return _db.Query<Contact>(sql).ToList();
```

On obtient :

```json
[
  {
    "id": 1,
    "fName": "Michael",
    "lastName": "Jordan",
    "email": "michael@bulls.com",
    "company": "Chicago Bulls",
    "title": "MVP",
    "isNew": false,
    "addresses": []
  },
```

