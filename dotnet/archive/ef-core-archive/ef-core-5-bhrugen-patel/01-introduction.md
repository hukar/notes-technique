# 01 Introduction `EF Core 5`

## `ORM` Object Relationnal Mapping

Un `ORM` crée un pont entre les objets d'une **application en orienté objet** et un système de **base de données relationnel**.

Un `ORM` permet de substituer l'écriture du `SQL` par l'utilisation d'une syntaxe orienté objet.

On peut utiliser son langage objet pour manipuler la base de données sans utiliser `SQL`.

### Avantages

- Un seul langage utilisé => simplicité
- C'est une `abstraction` qui permet de changer de base de données sans conséquence sur le code.
- Les requête sont optimisées par l'`ORM` => de meilleures performances
- C'est plus rapide à écrire (beaucoup de fonctionalités sont déjà implémentées)
- L'`ORM` est capable de générer la base de données.



## `EF Core`

- C'est le remplaçant de `EF 6.x`
- Il est `Open Source`,  léger et `Cross-Platform`

- C'est une amélioration de `ADO.NET`
- Utilise `Linq` pour l'écriture des requêtes



### avantages supplémetaires

- Génère les modèles depuis la `BDD` et vice versa
- Plus de sécurité
- Pas de `mapping` manuel
- Pas besoin de procédures stockées, mais on peut quand même les utiliser si besoin



## Créer la solution

On a l'arborescence suivante :

 `WizLib` (web MVC)

`WizLib.Data` (class library)

- `Data`

`WizLib.Model` (class library)

- `Models` (copié de MVC)



`WizLib` a une référence vers `WizLib.Data` et `WizLib.Model`

`WizLib.Data` a une référence vers `WizLib.Model`



## Ajouter `EF Core`

On installe `EF Core` dans les projets `WizLib` et `WizLib.Data`.

On installe la version `SqlServer`.

```bash
dotnet add package Microsoft.EntityFrameworkCore.SqlServer --version 5.0.7
```



## `Connection String`

On renseigne le `Connection String` dans le fichier ``appsettings.json``

```json
"AllowedHosts": "*",
"ConnectionStrings": {
  "HukarConnection": "Server=localhost,1433; Database=WizLib_db; User=sa; Password=huk@r2Xmen99"
}
```



## `DbContext`

Une instance de `DbContext` représente une session avec la `BDD`.

`DbContext` est une combinaison de `Unit Of Work` et `Repository Pattern`.

Fonctionnalités de `DbContext` :

- Gérer la connexion à la `BDD`
- Configurer le `model` et les relations
- `Requêter` la `BDD`
- `Sauver` les données en `BDD`
- Configurer la surveillance des changements (`tracking`)
- Mettre en cache (`caching`)
- Gérer les `transactions`

On a besoin de créer une classe dérivée de `DbContext` dans notre application.

Cette classe contient un `DbSet` pour chaque entité de l'application.



### Création de `ApplicationDbContext`

On va créer cette classe dans `WizLib.Data` dans le dossier `Data` :

```cs
namespace WizLib.data.Data
{
    public class ApplicationDbContext : DbContext
    {
        public ApplicationDbContext(DbContextOptions<ApplicationDbContext> options) : base(options)
    }
}
```

On a juste besoin de passer les options au constructeur `base`.

On va lier le context dans le fichier `Startup.cs` du projet `WizLib` :

```cs
public void ConfigureServices(IServiceCollection services)
{
  services.AddDbContext<ApplicationDbContext>(
    options => options.UseSqlServer(
    	Configuration.GetConnectionString("HukarConnection")
    ))
	// ...
```









