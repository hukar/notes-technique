# 01 Authentification

Il y a trois catégories pour authentifier quelqu'un :

1. Quelque chose qu'il connait : password, PIN, question de sécurité
2. Quelque chose qu'il a : token, carte, téléphone
3. Quelque chose qu'il est : empreinte digital, voix, reconnaissance faciale



## Mise en application

Un utilisateur sera capable de s'authentifier grâce à son nom, son email et un mot de passe.

On va créer une nouvelle `entity` : `User`

```cs
public class User
{
  public int Id { get; set; }
  public string Email { get; set; } = "";
  public string FirstName { get; set; } = "";
  public string LastName { get; set; } = "";
  public string Nationality { get; set; } = "";
  public DateTime? DateOfBirth { get; set; }
  public string PasswordHash { get; set; }
  public int RoleId { get; set; }
  public Role Role { get; set; }
}
```

Le `Password` est un `Hash` (une empreinte numérique produite à partir d'une fonction de `Hachage`).

`Role` est le rôle que joue une personne pour l'application.

On va créer l'`entity` : `Role`

```cs
public class Role
{
  public int Id { get; set; }
  public string RoleName { get; set; }
}
```

On va ensuite ajoute des `DbSet` au `MeetupContext`

```cs
public class MeetupContext : DbContext
{
  // ...

  public DbSet<Role> Roles { get; set; }
  public DbSet<User> Users { get; set; }
```



### Connection à la `DB`

Dans `MeetupContext.cs` il faut renseigner le `ConnectionString` :

```cs
private string _connectionString = "Server=localhost,1433; Database=identity-test-db; User=sa; Password=huk@r2Xmen99";
```



On crée ensuite une migration et on met à jour la `DB` :

```bash
dotnet ef migrations add AddingUserAndRole

dotnet ef database update
```

On ajoute ensuite trois `Role` dans la `DB`

```sql
INSERT INTO Roles Values ("User"), ("Moderator"), ("Admin")
```



## Création d'un `Controller` : `AccountController`

```cs
[Route("api/account")]
public class AccountController : ControllerBase
{
  private readonly MeetupContext _meetupContext;

  public AccountController(MeetupContext meetupContext)
  {
    _meetupContext = meetupContext;
  }

  [HttpPost("register")]
  public ActionResult Register([FromBody] RegisterUserDto registerUserDto)
  {
    if(!ModelState.IsValid)
    {
      return BadRequest(ModelState);
    }

    var newUser = new User {
      Email = registerUserDto.Email,
      Nationality = registerUserDto.Nationality,
      DateOfBirth = registerUserDto.DateOfBith,
      RoleId = registerUserDto.RoleId,
    };
  }
}
```

On crée le `DTO` dans le dossier `Models` :

`RegisterUserDto.cs`

```cs
public class RegisterUserDto
{
  [Required]
  public string Email { get; set; }
  [Required]
  [MinLength(6)]
  public string Password { get; set; }
  public string Nationality { get; set; }
  public DateTime? DateOfBith { get; set; }
  public int RoleId { get; set; } = 1;
}
```



## Password `Hashing`

**Microsoft** offre dans son package `System.Security.Cryptography.Algorithms` des fonctions de hachage secures.

Il y a aussi la classe `PasswordHasher` déjà dans `Microsoft.AspNetCore.Identity`.

Elle a deux méthodes :

### `HashPassword(TUser user, string password)`

Qui renvoie un `hashedPassword` (type `string`).

### `VerifyHashedPassword(TUser user, string hashedPassword, string providedPassword)`

Qui vérifie si le `password` fourni correspond et renvoie un `PasswordVerificationResult`.

On enregistre cette classe comme `service` :

```cs
services.AddScoped<IPasswordHasher<User>, PasswordHasher<User>>();
```

On peut maintenant l'injecter dans notre `Controller` :

```cs
private readonly MeetupContext _meetupContext;
private readonly IPasswordHasher<User> _passwordHasher;

public AccountController(MeetupContext meetupContext, IPasswordHasher<User> passwordHasher)
{
  _passwordHasher = passwordHasher;
  _meetupContext = meetupContext;
}
```

On va ensuite l'utiliser dans notre action `Register`

```cs
newUser.PasswordHash = _passwordHasher.HashPassword(newUser, registerUserDto.Password);

_meetupContext.Add(newUser);
```



## L'`email` doit être unique

On pourrait tester si l'email est unique directement dans le `Controller` :

### Utilisation de `Fluent Validation`

```bash
dotnet add package FluentValidation.AspNetCore --version 10.3.3
```

On enlève les validation par attribut de notre `RegisterUserDto` et on ajoute la propriété `confirmPassword` :

```cs
public class RegisterUserDto
{
  public string Email { get; set; }
  public string Password { get; set; }
  public string ConfirmPassword { get; set; }
  public string Nationality { get; set; }
  public DateTime? DateOfBith { get; set; }
  public int RoleId { get; set; } = 1;
}
```

`FluentValidation` va nous permettre de créer une classe responsable de la validation de `RegisterUserDto` :

`Validators/RegisterUserDtovalidator`

```cs
public class RegisterUserDtoValidator : AbstractValidator<RegisterUserDto>
{
  public RegisterUserDtoValidator(MeetupContext meetupContext)
  {
    RuleFor(u => u.Email)
      .NotNull()
      .NotEmpty()
      .EmailAddress()
      .Must((email) => {
        var userWithIdenticalEmail = meetupContext.Users.FirstOrDefault(u => u.Email == email);

        return userWithIdenticalEmail == null;
      })
      .WithMessage("{PropertyName} {PropertyValue} is already used");

    RuleFor(u => u.Password)
      .NotEmpty()
      .NotNull()
      .MinimumLength(6)
      .Equal(u => u.ConfirmPassword);
  }
}
```

On peut aussi utiliser un `Custom Validator` qui donne plus de contrôle pour le renvoie d'une erreur de validation avec `context.AddFailure` :

```cs
RuleFor(u => u.Email)
  	.Custom((emailValue, context) => {
      var userAlreadyExists = meetupContext.Users.Any(user => user.Email == emailValue);
      
      if(userAlreadyExists)
      {
        context.AddFailure("Email", "That email address is taken");
      }
    });
```

On peut greffer `FluentValidation` sur le `Controller` dans `Startup` :

```cs
services.AddControllers().AddFluentValidation();
services.AddScoped<IValidator<RegisterUserDto>, RegisterUserDtoValidator>();
```

> Pour tester l'`API` il y a `HttpRepl` de Microsoft :
>
> ```cs
> dotnet tool install -g Microsoft.dotnet-httprepl
> ```



## `JWT`

Le `token` se compose 

- D'un `header` contenant le type d'algorithme utilisé
- D'un `payload` avec tous les `claims` nécéssaire pour l'application (`Name`, `Role`, `Nationality`)
- Une `signature` qui authentifie le `token` et qui doit correspondre chez l'envoyeur sinon l'authentification échoue.



### Installation

Pour réaliser cette authentification par `token` on doit installer un package `Microsoft` :

```cs
dotnet add package Microsoft.AspNetCore.Authentication.JwtBearer --version 3.1.19
```



### Configuration

Dans `appsettings.json` :

```js
{ 
	"jwt": {
        "JwtKey": "JWT_KEY_MIN_LENGTH_16_CHARACTERS",
        "JwtIssuer": "http://meetup.com",
        "JwtExpireDays": 15
    },
    "Logging" : {
        // ...
```

`Issuer` : émetteur

On va créer une classe avec ces propriétés dans un dossier `Identity`.

```cs
public class JwtOptions
{
    public string JwtKey { get; set; }
    public string JwtIssuer { get; set; }
    public int JwtExpireDays { get; set; }
}
```



### Configuration dans `Startup`

Dans la méthode `ConfigureServices`

```cs
public void ConfigureServices(IServiceCollection services)
{
    var jwtOptions = new JwtOptions();
    Configuration.GetSection("jwt").Bind(jwtOptions);

    services.AddSingleton(jwtOptions);

    services.AddAuthentication(options => {
        options.DefaultAuthenticateScheme = "Bearer";
        options.DefaultScheme = "Bearer";
        options.DefaultChallengeScheme = "Bearer";
    }).AddJwtBearer(cfg => {
        cfg.RequireHttpsMetadata = false;
        cfg.TokenValidationParameters = new TokenValidationParameters {
            ValidIssuer = jwtOptions.JwtIssuer,
            ValidAudience = jwtOptions.JwtIssuer,
            IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(jwtOptions.JwtKey))
        };
    })
```

Dans la méthode `Configure`

```cs
	// ...
	app.UseHsts();
}

app.UseAuthentication();
app.UseHttpsRedirection();
// ...
```





















