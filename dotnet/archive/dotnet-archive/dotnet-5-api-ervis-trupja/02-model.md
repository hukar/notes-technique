# 02 `Models`

## `EDM`

`EF Core` utilise `EDM` : `Entity Data Model` pour toutes ses opérations avec la base de données.

`EDM` ets un modèle qui décrit l'entité ainsi que les relations qu'elle établi.

### Les `Models` sont justes des classes `C#`

On crée un nouveau dossier `Data` et dedans `Models`.

Dans `Models` on va créer une classe `Book.cs` :

```cs
public class Book
{
  public int Id { get; set; }
  public string Title { get; set; }
  public string Description { get; set; }
  public bool IsRead { get; set; }
  public DateTime? DateRead { get; set; }
  public int? Rate { get; set; }
  public string Author { get; set; }
  public DateTime DateAdded { get; set; }
  public string CoverUrl { get; set; }
}
```

Une propriété optionnelle est notée `type?`.

> ### Lire les propriétés d'un objet
>
> ```cs
> using System.ComponentModel;
> 
> foreach(PropertyDescriptor descriptor in TypeDescriptor.GetProperties(obj))
> {
>     string name=descriptor.Name;
>     object value=descriptor.GetValue(obj);
>     Console.WriteLine("{0}={1}",name,value);
> }
> ```
>
> 

