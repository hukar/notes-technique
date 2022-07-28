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



## Insérer plusieurs fois la même valeur `GO n`

```sql
CREATE TABLE Classique (Numero int)

INSERT INTO Classique (numero) values (1)
GO 10 -- répète 10 fois la commande
```



## Effacer des données `DELETE`

```sql
DELETE FROM ma_table
```

```sql
DELETE FROM Nom WHERE Prenom = 'Dagobert'
```



## Exercice

### Création : `CREATE TABLE`

```sql
CREATE TABLE Etudiant (
    Id int IDENTITY(1,1) NOT NULL
    CONSTRAINT PK_Etudiant_Id PRIMARY KEY,
    Nom varchar(200),
    Prenom char(10),
    Age int,
)
```



### Ajout de données : `INSERT INTO`

```sql
INSERT INTO Etudiant (Nom, Prenom, Age) 
VALUES
('Rock','Immap', 23),
('Tana','Lee', 31),
('Boris','Grasouille', 65),
('Piotr','Mallassus', 42),
('Rena','Lapidou', 19)
```



### Mettre à jour : `UPDATE`

> #### ! pas de parenthèse !

```sql
UPDATE Etudiant SET Age = 20 WHERE Nom = 'Rena'
```



### Supprimer : `DELETE`

```sql
DELETE FROM Etudiant WHERE Nom = 'Piotr'
```

