# 17 Test d'intégration

Le meilleur moyen de tester sa `DB` avec `EF Core` c'est d'utiliser les tests d'intégration.

On utilise `XUnit` pour créer un projet de `Tests`.

On va créer une classe `databaseFixture` :

```cs
public class DatabaseFixture : IDisposable
{
    public MyAppContext Context { get; }
    
    public DatabaseFixture()
    {
        var factory = new MyAppContextFactory();
        Context = factory.CreateDbContext();
        
        Context.Database.EnsureDeleted();
        Context.Database.EnsureCreated();
    }
    
    public void Dispose()
    {
        Context.Dispose();
    }
}
```

Attention comme on recrée la `DB` à chaque création du `Context`, il faut utiliser une `DB` réservée aux `Tests` :

`appsettings.json`

```json
"ConnectionStrings": {
    "MSSqlConnectTest": "Server=localhost,1433;Database=share-for-futureTest;User=sa; Password=huk@r2Xmen99"
  },
```



## Création du test `UsersTests`

```cs
public class UsersTests : IClassFixture<DatabaseFixture>
{
    private readonly DatabaseFixture _fixture;
    
    public UsersTests(DatabaseFixture fixture)
    {
        _fixture = fixture;
        // essentiel sinon erreur avec duplicate id pour les UserGroups
        _fixture.Context.ChangeTracker.Clear();
    }
    
    [Fact]
    [Trait("Category", "IntegrationTest")]
    public async Task DeleteGroup()
    {
        var newUser = new User { 
            FirstName = "Jonh", 
            LastName = "Doe",
        	UserGroupId = 1
        };
        
        Assert.Equal(3, await fixture.Context.UserGroups.CountAsync());
        
        // ADD USER
        fixture.Context.Users.Add(newUser);
        await fixture.Context.SaveChangesAsync();
        fixture.Context.ChangeTracker.Clear();
        
        // Try to remove group that has user -> must not work
        var group = await fixture.Context.UserGroups
            .FirstAsync(g => g.Id == newUser.UserGroupId);
        fixture.Context.UserGroups.Remove(group);
        await Assert
        .ThrowsAsync<DbUpdateException>(
            async () => await fixture.Context.SaveChangesAsync()
        );
        
        // Ensure that group is still in DB
        Assert.Equal(3, await fixture.Context.UserGroups.CountAsync());
    }
}
```

On appelle cela un `test d'intégration` car le test a accès à une vrai `DB`.



## Autre exemple de test :

On teste ici la contrainte d'unicité de l'`email` : 

```cs
builder.HasIndex(u => u.ConatctEmailAddress).IsUnique();
```

Voici le test :

```cs
[Fact]
[Trait("Category", "IntegrationTest")]
public async Task DuplicateEmail()
{
	var newUser1 = new User { FirstName = "Georges", ContactEmailAddress = "go@gmail.com"};
    var newUser2 = new User { FirstName = "Gontran", ContactEmailAddress = "go@gmail.com"};
    
    // Add user one 
    fixture.Context.Users.Add(newUser1);
    await fixture.Context.SaveChangesAsyn();
    
    // Add user two
    fixture.Context.Users.Add(newUser2);
    
    // Make sure that user with identicat email has not been inserted
    await Assert.ThrowsAsync<DbUpdateException>(
    	async () => await fixture.Context.SaveChangesAsync()
    );
    Assert.Equal(1, await fixture.Context.Users.CountAsync());
}
```



## Exemple test pour la contrainte de Période

(voir fiche `16-fluent-api` pour la contrainte ajoutée avec `HasCheckConstraint`)

### `dateTime.Now.AddDays(intValue)`

```cs
[Fact]
[Trait("Category", "IntegrationTest")]
public async Task InvalidUnavailabilityPeriod()
{
    // Add Incalid Period
    fixture.Context.Periods.Add(new() {
        From = DateTime.Now.AddDays(-1),
        Until = DateTime.Now.AddDays(-5)
    });
    
    // Make sure that we cannot insert an invalid period
    await Assert.ThrowsAsync(
    	async() => await fixture.Context.SaveChangesAsync()
    );
}
```

