# 01 installer `Dapper` et `Sqlite` ensemble



## Installer les dépendances

```bash
dotnet add package Micrisift.Data.Sqlite

dotnet add package dapper
```





## Créer une `DB` dans un fichier

> ### `In Memory`
>
> Avec `Sqlite` on peut aussi créer la `DB` juste en mémoire plutôt que dans un fichier :
>
> ```cs
> Data Source=:memory:
> ```
>
> Partagée entre plusieurs connexion :
>
> ```cs
> Data Source=InMemorySample;Mode=Memory;Cache=Shared
> ```
>
> La `DB` persiste tant qu'il reste au moins une connexion ouverte.

Il suffit de créer une connexion et de l'ouvrir pour créer un fichier `DB`.Il suffit de créer une connexion et de l'ouvrir pour créer un fichier `DB`.

```cs
using var conn = new SqliteConnection("Data Source=robot.db");

conn.Open();
```

<img src="assets/db-in%20-file-sqlite.png" alt="db-in -file-sqlite" style="zoom:50%;" />