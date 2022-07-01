# 02 Setting up



## Créer les classes `Model`

`Contact.cs`

```cs
public class Contact
{
    public int Id { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string Email { get; set; }
    public string Company { get; set; }
    public string Title { get; set; }
    public bool IsNew => this.Id == default(int);
    public List<Address> Addresses { get; } = new List<Address>();
}
```

`Address.cs`

```cs
public class Address
{
    public int Id { get; set; }
    public int ContactId { get; set; }
    public string AddressType { get; set; }
    public string StreetAddress { get; set; }
    public string City { get; set; }
    public int StateId { get; set; }
    public string PostalCode { get; set; }
    internal bool IsNew => (this.Id == default(int));
    public bool IsDeleted { get; set; }
}
```





## création de l'interface du `Repository`

`IContactRepository`

```cs
public interface IContactRepository
{
    Contact GetById(int id);
    List<Contact> GetAll();
    Contact Add(Contact contact);
    Contact Update(Contact contact);
    void Delete(int id);
}
```

