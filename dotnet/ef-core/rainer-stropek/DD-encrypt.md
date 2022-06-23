# DD `Encrypt`

Par défaut `Encrypt` est réglé sur `True` dans `SQLServer`.

J'ai obtenu une erreur :

<img src="assets/error-encrypt-true-sqlserver.png" alt="error-encrypt-true-sqlserver" style="zoom:50%;" />

On règle cela dans `appsettings.json`

```json
{
    "ConnectionStrings": {
        "SqliteConnection": "DataSource=MinimalApi.db",
        "HukarConnection": "Server=localhost,1433;Database=MinimalAPI2;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true;Encrypt=False"
    }
}
```

On a ajouté `Encrypt=False`.

Ce n'est pas une bonne pratique, mais le problème disparaît.

On peut aussi ajouter `TrustServerCertificate=True` ce qui n'est pas non plus une bonne pratique de sécurité

> ## À approfondir ???
>
> - Provoquer par une version `preview` de `EF Core` : `--version 7.0.0-preview.1.22076.6`
>   Tout revient à la normal avec la `--version 6.0.3`, plus besoin d'ajout dans le `Connection String`.

## Regénérer ses certificats

Si besoin on peut regénérer ses certificats :

```bash
dotnet dev-certs https --clean
dotnet dev-certs https # génère les certificats
dotnet dev-certs https --trust
```

