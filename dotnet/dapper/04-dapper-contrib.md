# 04 `Dapper Contrib`

https://github.com/DapperLib/Dapper.Contrib

Apporte des méthodes pour écrire moins de code dans le cas d'un `CRUD`.



## Ajouter `Dapper Contrib`

```bash
dotnet add package Dapper.Contrib
```



## Méthodes disponibles

Une version`Async` existe pour chaque `méthode` :

```cs
T Get<T>(id);
IEnumerable<T> GetAll<T>();
int Insert<T>(T obj);
int Insert<T>(Enumerable<T> list);
bool Update<T>(T obj);
bool Update<T>(Enumerable<T> list);
bool Delete<T>(T obj);
bool Delete<T>(Enumerable<T> list);
bool DeleteAll<T>();
```

`T` doit avoir obligatoirement un `Id`.

Soit `Id` par convention, soit l'attribut `[Key]` :

```cs
public class MyEntity
{
    [Key]
    public int MyCustomKey { get; set; }
	// ...
```



## Création du `ContactContribRepository`

