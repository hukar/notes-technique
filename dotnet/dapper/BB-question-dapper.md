## BB Question à l'équipe de `Dapper`



## Cas avec `Query`

Si j'utilise purement `ADO.NET`, je vais devoir utiliser trois fois `using` pour utiliser un `reader` :

```cs
using var cnn = new SqlConnection(cnnStr);
using var cmd = new SqlCommand(sql, cnn);

cnn.Open();

using SqlDataReader dr = cmd.ExecuteReader(CommandBehavior.CloseConnection);

while(dr.Read()) {
    // ...
```

Avec `Dapper` un seul `using` semble nécessaire en regardant votre documentation :

```cs
using var cnn = new SqlConnection(cnnStr);

var dogs = cnn.Query<Dog>("select * from Dogs");
```

Si je regarde l'implémentation de `Query<T>` je trouve :

```cs
private static IEnumerable<T> QueryImpl<T>(this IDbConnection cnn, CommandDefinition command, Type effectiveType)
{
    // ...

    IDbCommand cmd = null;
    IDataReader reader = null;

    // ...
    try
    {
        cmd = command.SetupCommand(cnn, info.ParamReader);

        // ...
        reader.Dispose();
        // ...
    }
    finally
    {
        if (reader != null)
        {
            // ...
            reader.Dispose();
        }
        if (wasClosed) cnn.Close();

        // ...
        cmd?.Dispose();
    }
}
```

Ce qui est cohérent et remplace donc l'utilisation de `using`.



Maintenant un peu plus loin dans cotre documentation je trouve :

```cs
using (var multi = connection.QueryMultiple(sql, new {id=selectedId}))
{
   var customer = multi.Read<Customer>().Single();
   var orders = multi.Read<Order>().ToList();
   var returns = multi.Read<Return>().ToList();
   ...
}
```

Et 

```cs
using (var reader = connection.ExecuteReader("select * from Shapes"))
{
    // ...
```

> ## Auto-réponse 
>
> Ces deux méthodes sont les seuls à renvoyer des objets implémentant `IDisposable` :
>
> ```cs
> private static GridReader QueryMultipleImpl(this IDbConnection cnn, ref CommandDefinition command)
> { 
>     // ...
>     
> ```
>
> ```cs
> public partial class GridReader : IDisposable
> {
>     // ...
> ```
>
> De même `ExecuteReader` :
>
> ```cs
> private static IDataReader ExecuteReaderImpl(...)
> {
>     // ...
> ```
>
> ```cs
> public interface IDataReader : IDataRecord, IDisposable
> {
>     // ...
> ```
>
> 



## Réponses envoyée

Je viens de comprendre (enfin !), la pluspart des méthodes d'extension de `Dapper` implémente directement les appelles à `Dispose`. Il n'y a donc aucunes raisons d'utiliser un `using block`.

Par contre `QueryMultiple` et `ExecuteReader` sont les deux seules méthodes renvoyant des objets implémentant `IDisposable` :

```cs
private static GridReader QueryMultipleImpl(this IDbConnection cnn, ref CommandDefinition command)
{ 
    // ...
    
```

```cs
public partial class GridReader : IDisposable
{
    // ...
```

```cs
private static IDataReader ExecuteReaderImpl(...)
{
    // ...
```

```cs
public interface IDataReader : IDataRecord, IDisposable
{
     // ...
```


C'est pour cela qu'un `using block` est nécessaire dans ces cas de figure.



Merci et bravo pour votre librairie.