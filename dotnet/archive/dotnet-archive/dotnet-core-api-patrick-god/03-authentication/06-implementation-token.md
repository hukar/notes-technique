# 06 Implémentation des `JWT`

## Ajout d'un `secret` dans `appsettings.json`

```json
{
  "ConnectionStrings": {
    "HukarConnection": "Server=localhost,1433; Database=dotnet-rpg; User=sa; Password=huk@r2Xmen99"
  },
  "AppSettings": {
    "Token": "My very sercret hukar scret "
  },
    // ...
```

Le secret doit avoir minimun 16 caractères.



## Ajout d'une méthode `CreateToken` dans `AuthRepository.cs`

```cs
using System.Collections.Generic;
using System.Security.Claims;
using Microsoft.IdentityModel.Tokens;
using System.IdentityModel.Tokens.Jwt;
// ...

private string CreateToken(User user)
{
    var claims = new List<Claim>
    {
        new Claim(ClaimTypes.NameIdentifier, user.Id.ToString()),
        new Claim(ClaimTypes.Name, user.Username)
    };
    
    var key = new SymmetricSecurityKey(System.Text.Encoding.UTF8.GetBytes(
    	_configuration.GetSection("AppSettings:Token").Value
    ));
    
    var creds = new SigningCredentials(key, securityAlgorithms.HmacSha512Signature);
    
    var tokenDescriptor = new SecurityTokenDescriptor
    {
        Subject = new ClaimsIdentity(claims),
        Expires = System.DateTime.Now.AddDays(1),
        SigningCredentials = creds
    }
    
    var tokenHandler = new JwtSecurityTokenHandler();
    var token = tokenHandler.CreateToken(tokenDescriptor);
    
    // return string.Empty;
    return tokenHandler.WriteToken(token);
}
```

1. Les `claims` sont des informations données au `JWT`, ils sont en nombre limité et suivent un standard.

    Avec ce `token` on aura le `user.Id` et son `username` , mais aucune information sur le `password`.

2. On crée une clé symétrique avec `SymmetricSecurityKey`.

3. On crée une signature avec la clé précédente grâce à `SigningCredentials`.

4. On doit aussi créer un `tokenDescriptor`
5. Le `tokenHandler` va pouvoir créer le `token` ...
6. ... et le transformer en `string` avec `WriteToken`.



On doit récupérer notre `secret` en injectant la configuration dans le constructeur de `AuthRepository` :

```cs
using Microsoft.Extensions.Configuration;
// ...

private readonly IConfiguration _configuration;

public AuthRepository(DataContext context, IConfiguration configuration)
{
    _configuration = configuration;
    _context = context;
}
```



## Modification de la méthode `Login`

Dans `AuthRepository`

```cs
public async Task<ServiceResponse<string>> Login(string username, string password)
        {
            var response = new ServiceResponse<string>();

            var user = await _context.Users.FirstOrDefaultAsync(u => u.Username.ToLower().Equals(username.ToLower()));

            if (user is null)
            {
                response.Success = false;
                response.Message = "User not found";
            }
            else if (!VerifyPasswordHash(password, user.PasswordHash, user.PasswordSalt))
            {
                response.Success = false;
                response.Message = "Wrong password";
            }
            else
            {
                // response.Data = user.Id.ToString();
                response.Data = CreateToken(user);
            }

            return response;
        }
```



## Test avec `Swagger`

<img src="assets/swagger-test-auth-token.png" alt="swagger-test-auth-token" style="zoom:50%;" />

<img src="assets/effective-token-returned-by-api.png" alt="effective-token-returned-by-api" style="zoom:50%;" />

```
"eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJuYW1laWQiOiIxIiwidW5pcXVlX25hbWUiOiJodWthciIsIm5iZiI6MTYxODU2NzYwMSwiZXhwIjoxNjE4NjU0MDAxLCJpYXQiOjE2MTg1Njc2MDF9.1ADgQ5brpCknagUsDbe86P2U0J3IbGT5JDAIQEnTeBLP8n-ZYifZ80UsXvPnPKPRNcq96tGvTtuAlKUppLHnLA"
```

### Lire son token : `jwt.io`

<img src="assets/jwt-io-token-read.png" alt="jwt-io-token-read" style="zoom:50%;" />

On peut vérifier son `token` sur le site https://jwt.io/

