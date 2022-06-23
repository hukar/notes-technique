# 05 Server Side `Validation`

## Modèle Validation : Définition

S'assurer qu'un programme fonctionne avec des données propres, correctes et utiles.

Vérifier l'exactitude, la pertinence et la sécurité des données introduites.

<img src="assets/no-trust-data-entries.png" alt="no-trust-data-entries" style="zoom:50%;" />

Ne jamais faire confiance aux données entrantes.

#### La `validation` de données ne se substitue pas aux règles business.

Elle permet d'être sûr d'appliquer les règles business sur des données correctes.



## Règles sur le schéma de Base de Données

### Annotation dans les entités

Il est possible de mettre des annotations sur les propriétés directement :

```cs
using System.ComponentModel.DataAnnotations;

// ...

[Required]
public string FileNumber { get; set; }
```

Mais souvent pour des raisons de séparation des résponsabilités on préfère laisser les entités vierge de validation.



### `ModelBuilder` dans la classe `Context`

Lorsqu'on définit le `context` d'`EF Core` , on peut définir des contrainte sur nos champs en base de données grâce au `ModelBuilder` :

```cs
protected override void OnModelCreating(ModelBuilder bldr)
{
  base.OnModelCreating(bldr); // Do the Default

  bldr.Entity<Case>()
    .Property(c => c.FileNumber)
    .IsRequired()
    .HasMaxLength(50);
```

Ce sont des indications pour la base de données (on utilise une `Fluent syntax` = syntaxe chaînée).

Pour appliquer les changements au schéma de la `base de données`, on va utiliser les `migrations` :

```bash
dotnet ef migrations ass SchemaChanges -p ../JurisTempus.Data -o ../JurisTempus.data/Migrations
```

Il faut lancer cette commande dans le projet `JurisTempus` (certainement parceque le package `EF Designer` est installé là).

Maintenant on peut appliquer la `migration` en sachant que :

```
An operation was scaffolded that may result in the loss of data. Please review the migration for accuracy.

Une opération a été préparée qui peut entraîner la perte de données. Veuillez revoir la migration pour plus de précision.
```

```bash
dotnet ef database update
```



## Règles sur un objet `ViewModel`

On va placer des règles grâce aux annotations usr l'objet `ViewModel` : `CaseViewModel`.

Ainsi on est sûr que les données envoyées en `BDD` sont correctes.

`CaseViewModel`

```cs
public class CaseViewModel
{
  public int Id { get; set; }
  [Required]
  [MinLength(9)]
  [MaxLength(50)]
  public string FileNumber { get; set; }
  [Required]
  public CaseStatus Status { get; set; }
}
```

`[MinLength(9)]` est une validation intéressante qui n'a pas besoin d'être répercutée directement sur la structure de la `BDD`.

Maintenant pour des `validations` plus complexe, il vaut mieux utiliser `Fleunt Validation`.

