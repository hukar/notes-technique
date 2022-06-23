# AA Problème avec `Encrypt`

On a parfois un problème avec une erreur de type :

```
System.Security.Authentication.AuthenticationException: The remote certificate was rejected by the provided RemoteCertificateValidationCallback.
```

En cherchant on trouve sur le web :

https://github.com/dotnet/SqlClient/issues/1402

Il faut modifier la `ConnectionStrings` avec `Encrypt=false` (la valeur par défaut étant `true`).

`appsettings.json`

```cs
{
    "ConnectionStrings": {
        "HukarConnect": "Server=localhost,1433;Database=NewDapperTest;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true;Encrypt=false"
    },
```

