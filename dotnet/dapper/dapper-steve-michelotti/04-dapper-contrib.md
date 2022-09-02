# 04 `Dapper Contrib`

https://github.com/DapperLib/Dapper.Contrib

Apporte des méthodes pour écrire moins de code dans le cas d'un `CRUD`.



## Ajouter `Dapper Contrib`

```bash
dotnet add package Dapper.Contrib
```



## Méthodes disponibles

Une version`Async` existe pour chaque `méthode` :

```cs
T Get<T>(id);
IEnumerable<T> GetAll<T>();
int Insert<T>(T obj);  // return Id
int Insert<T>(Enumerable<T> list); // return rowsAffected
bool Update<T>(T obj);
bool Update<T>(Enumerable<T> list);
bool Delete<T>(T obj);
bool Delete<T>(Enumerable<T> list);
bool DeleteAll<T>();
```

`T` doit avoir obligatoirement un `Id`.

Soit `Id` par convention, soit l'attribut `[Key]` :

```cs
public class MyEntity
{
    [Key]
    public int MyCustomKey { get; set; }
	// ...
```



## Création du `ContactContribRepository`

```cs
public class ContactContribRepository : IContactRepository
{
    private readonly IDbConnection _db;
    public ContactContribRepository(SqlConnection connection)
    {
        _db = connection;
        Console.WriteLine("Contact Contrib Repository in da place !!!");
    }

    public Contact GetById(int id) => _db.Get<Contact>(id);

    public List<Contact> GetAll() => _db.GetAll<Contact>().ToList();

    public Contact Add(Contact contact)
    {
        var id = _db.Insert<Contact>(contact);
        contact.Id = (int)id;

        return contact;
    }

    public UpdateContactDto Update(Contact contact)
    {
        var isUpdated = _db.Update<Contact>(contact);

        return new UpdateContactDto(contact, Convert.ToInt32(isUpdated));
    }

    public int Delete(int id)
    {
        // Delete de Dapper.Contrib reçoit un contact en paramètre
        // Il suffit de créer un contact vode avec l'Id désiré
        var isDeleted = _db.Delete<Contact>(new Contact { Id = id });

        return Convert.ToInt32(isDeleted);
    }
}
```

`Insert` retourne l'`Id` de l'enregistrement ou le nombre de ligne ajouté si on lui passe une liste.

## Erreur obtenu

En effectuant des requêtes on obtient cette erreur :

```
Microsoft.Data.SqlClient.SqlException (0x80131904): Invalid column name 'IsNew'.
Invalid column name 'Addresses'.
   at Microsoft.Data.SqlClient.SqlConnection.OnError(SqlException exception, Boolean breakConnection, Action`1 wrapCloseInAction)
```

En fait `IsNew` et `Address` sont des propriétés calculées de `Contact`, elle ne doivent pas être sauvegardées en `DB`.

On utilise l'attribut `[Computed]` (fournit par `Dapper Contrib`) :

Pour `Addresses` on place l'attribut `[Write(false)]` car ce n'est pas une propriété calculée, mais elle n'existe pas en `DB`.

```cs
using Dapper.Contrib.Extensions;

public class Contact
{
    // ...
    [Computed]      
    public bool IsNew => this.Id == default(int);
    [Write(false)]
    public List<Address> Addresses { get; } = new List<Address>();
```

