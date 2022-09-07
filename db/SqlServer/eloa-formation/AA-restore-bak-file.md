# AA Restaurer une `db` depuis un fichier `.bak`

Grâce à l'article suivant :

https://database.guide/how-to-restore-a-sql-server-database-on-a-mac-using-sql-operations-studio/

## Installer la `db`

Il faut copier le fichier `.bak` sur le `file system` du container `Docker` et non pas dans le `FS` de `macOS`.

Créer un dossier dans le conteneur `Docker`

```bash
sudo docker exec -it sql_dotnet1 mkdir /var/opt/mssql/backup
```

`sql_dotnet1` étant le nom donné avec `--name` lors de la création du conteneur.



Copier le fichier `.bak` dans ce dossier

```bash
sudo docker cp AdventureWorks2014.bak sql_dotnet1:/var/opt/mssql/backup
```



> Après plusieurs échecs en `TSQL`, j'ai utilisé l'interface d'`Azure Data Studio`

<img src="assets/step-one-restore-ttg.png" alt="step-one-restore-ttg" style="zoom:50%;" />

<img src="assets/step-two-restore-yyh.png" alt="step-two-restore-yyh" style="zoom:50%;" />

<img src="assets/step-three-restore-daf.png" alt="step-three-restore-daf" style="zoom:50%;" />

<img src="assets/step-four-restore-ddn.png" alt="step-four-restore-ddn" style="zoom:50%;" />





## Script `TSQL` qui n'ont pas fonctionné

```sql
restore database AdventureWorks2014 
from disk = '/Users/hukar/Documents/SQLServerTemp/AdventureWorks2014.bak'
with replace,
move 'AdventureWorks2014_Data' to '/Users/hukar/Documents/SQLServerTemp/AdventureWorks2014.mdf',
move 'AdventureWorks2014_Log' to '/Users/hukar/Documents/SQLServerTemp/AdventureWorks2014.ldf'
```

```sql
USE [master]
RESTORE DATABASE [AdventureWorks2014]
FROM DISK = '/var/opt/mssql/backup/AdventureWorks2014.bak'
WITH MOVE 'AdventureWorks2014' TO '/var/opt/mssql/data/AdventureWorks2014.mdf',
MOVE 'AdventureWorks2014_log' TO '/var/opt/mssql/data/AdventureWorks2014.ldf',
FILE = 1,  NOUNLOAD,  STATS = 5
GO
```



## Script `TSQL` généré par `Azure Data Studio`

Si on demande le script dans `Azure Data Studio` on obtient :

```sql
USE [master]
RESTORE DATABASE [AdventureWorks2014] 
FROM  DISK = N'/var/opt/mssql/backup/AdventureWorks2014.bak' 
WITH  FILE = 1,  
MOVE N'AdventureWorks2014_Data' TO N'/var/opt/mssql/data/AdventureWorks2014_Data.mdf',  MOVE N'AdventureWorks2014_Log' TO N'/var/opt/mssql/data/AdventureWorks2014_Log.ldf',  NOUNLOAD,  STATS = 5

```

