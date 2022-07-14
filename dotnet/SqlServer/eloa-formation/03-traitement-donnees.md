# 03 Jouer avec les données

## Insérer des données `INSERT`

```sql
INSERT INTO Ma_Table VALUES ('coco')
```



```sql
CREATE TABLE Nom (Nom varchar(200), Prenom varchar(200))

INSERT INTO Nom VALUES ('Peter', 'Parker')

INSERT INTO Nom VALUES ('Sam', 'Francis'), ('Mark', 'Rothko')
```

Le `INTO` n'est pas obligatoire.



## Sélectionner des données `SELECT` et filtrer `WHERE`

```sql
SELECT * FROM ma_table

SELECT * FROM ma_table WHERE nom = 'hukar'
```



```sql
USE EloaBdd
GO

SELECT * FROM Nom

SELECT Nom AS p, Prenom AS pn FROM Nom

SELECT * FROM Nom WHERE Nom = 'Peter'
```



## Mettre à jour des données `UPDATE`

```sql
UPDATE ma_table SET colonne1 = 'hukar'
```



```sql
SELECT * FROM Nom

UPDATE Nom SET Prenom = 'Dague Noire' WHERE Nom = 'Parker'

UPDATE Nom Set Nom = 'Unknow'
```

#### ! Il faut en général mettre un `WHERE` pour ne pas tout modifier.

```sql
UPDATE Nom SET Prenom = 'Hukar', Nom = 'Dark Master' WHERE Nom = 'Francis'
```



> ## Les commentaires et les crochets
>
> `--` permet de mettre des commentaires
>
> ```sql
> -- Hey un commentaire
> 
> ```
>
> 
>
> ### Les crochets `[ ]`
>
> Si le nom de colonne a des espaces :
>
> ```sql
> SELECT [la super colonne avec espaces] FROM ma_table
> ```
>
> Pour utiliser les mots réservés comme nom de colonne :
>
> ```sql
> CREATE TABLE Reservation (table varchar(30)) -- pas possible
> CREATE TABLE Reservation ([table] varchar(30)) -- Ok
> 
> INSERT INTO Reservation table VALUES ('ma table') -- pas possible
> INSERT INTO Reservation ([table]) VALUES ('ma table') -- Ok
> 
> -- de même avec SELECT
> SELECT [table] FROM Reservation
> ```
>
> 



## Effacer des données `DELETE`

