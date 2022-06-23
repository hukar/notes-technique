# 03 Connection à une db `SQL Server`

## Dans `appSettings.json`

```json
{
    "ConnectionStrings": {
        "HukarConnection": "Server=localhost,1433; Database=dotnet-rpg; User=sa; Password=huk@r2Xmen99"
    },
    // ...
}
```

`"ConnectionStrings"` suit une convention de nommage utilisée par `Startup.cs`.

`"HukarConnection"` nom arbitraire.

Plutôt que `User=xxx; Password=xxx` on pourrait utiliser `integrated security=true`, ce qui laisse l'authentification au `Windows` qui héberge `SQL Server`.

> On peut avoir un problème si on exécute une requête tout en itérant sur les résultats d'une autre requête.
>
> Pour éviter cela on utilise `MultipleActiveResultSets=true` :
>
> ```json
> "ConnectionStrings": {
>         "HukarConnection": "Server=localhost,1433;Database=InAndOut;User=sa;Password=huk@r2Xmen99;MultipleActiveResultSets=true"
>     },
> ```



## Dans `Startup.cs`

```cs
public void ConfigureServices(IServiceCollection services)
{
  services.AddDbContext<DataContext>(options => options.UseSqlServer(Configuration.GetConnectionString("HukarConnection")));
  // ...
}
```



## Extension `SQL Server` pour VSCode

<img src="assets/sqlserver-vscode-three-records.png" alt="sqlserver-vscode-three-records" style="zoom:50%;" />

Il faut juste répondre aux questions pour se connecter :

<img src="assets/step-1.png" alt="step-1" style="zoom:50%;" />

Pas besoin de renseigner le `port`.

<img src="assets/step-2.png" alt="step-2" style="zoom:50%;" />

<img src="assets/step-3.png" alt="step-3" style="zoom:50%;" />

Ce sont les mêmes crédentials de la `connection string`.

<img src="assets/step-4.png" alt="step-4" style="zoom:50%;" />

Maintenant on peut requête et afficher sa `BDD` directement dans `VSCode` :

<img src="assets/connection-ready.png" alt="connection-ready" style="zoom:50%;" />



## Connexion `Azure Data Studio`

C'est la même chose :

<img src="assets/azure-studi-connexion.png" alt="azure-studi-connexion" style="zoom:50%;" />

Et on clic sur `connect`.

