# 08 `Dapper` et les procédures stockées

On va créer un nouveau `repository ` :

## `ContactRepositorySP`

`SP` pour `Stored Procedure`.

```cs
public class ContactRepositorySP : IContactRepository
{
    private readonly SqlConnection _con;
    public ContactRepository(SqlConnection con)
    {
        // On utilise la DI pour recevoir la connection           
        _con = con;    
    }
```

### `GetFullContact`

```cs
public Contact GetFullContact(int id)
{
    using var multiResults = _con.QueryMultiple("GetContact", new { id },commandType: CommandType.StoredProcedure);

        var contact = multiResults.Read<Contact>().SingleOrDefault();
        var addresses = multiResults.Read<Address>().ToList();

        if(contact is not null && addresses is not null)
        {
            contact.Addresses.AddRange(addresses);
        }

        return contact;
}
```



### `GetById`

```cs
public Contact GetById(int id) => _con.QuerySingleOrDefault<Contact>("GetContact", new { Id = id }, commandType: CommandType.StoredProcedure).SingleOrDefault();
```





> à faire ... avec les `dynamics parameters`

```cs
    
    public Contact Add(Contact contact)
    {
        throw new NotImplementedException();
    }
    
    public void Delete(int id)
    {
        throw new NotImplementedException();
    }
    
    public List<Contact> GetAll()
    {
        throw new NotImplementedException();
    }
    
    public List<Contact> GetAllFullContact()
    {
        throw new NotImplementedException();
    }
    
    public Contact GetById(int id)
    {
        throw new NotImplementedException();
    }
    
    
    
    public void Save(Contact contact)
    {
        throw new NotImplementedException();
    }
    
    public Contact Update(Contact contact)
    {
        throw new NotImplementedException();
    }
}
```

