# 11. Création d'un attribut à usage multiple

<img src="assets/Screenshot2020-10-23at16.54.36.png" alt="Screenshot 2020-10-23 at 16.54.36" style="zoom:50%;" />

## Création de la classe `IndentAttribute`

```csharp
using System;

namespace csharp_attributes
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = true)]
    public class IndentAttribute : Attribute {}
}
```

## Modification de `WriteFirstName` dans la. classe `ContactConsoleWriter.cs`

```csharp
public void WriteFirstName()
{
    PropertyInfo firstNameProperty = typeof(Contact).GetProperty(nameof(Contact.FirstName));
	// ...

    IndentAttribute[] indentAttributes = (IndentAttribute[])Attribute.GetCustomAttributes(firstNameProperty, typeof(IndentAttribute));

    PreserveForgroundColor();

    StringBuilder sb = new StringBuilder();

    if(indentAttributes != null)
    {
        foreach(var a in indentAttributes)
        {
            sb.Append(new String(' ', 4));
        }
    }
    // ...
```

### Reflection

On récupère un `PropertyInfo` pour le `FirstName` :

<img src="assets/Screenshot2020-10-24at07.56.18.png" alt="Screenshot 2020-10-24 at 07.56.18" style="zoom:50%;" />

Comme l'`attribute` peut être multiple, on, utilise `Attribute.GetCustomAttributes` avec un `s`.

Cette méthode renvoie un tableau de `CustomAttribute`.

## Dans la class `Contact.cs`

```csharp
 public class Contact
 {
     [Display("First Name :", System.ConsoleColor.Cyan)]
     [Indent]
     public string FirstName { get; set; }

     [DebuggerBrowsable(DebuggerBrowsableState.Never)]
     public int AgeInYears { get; set; }
 }
```

<img src="assets/Screenshot2020-10-24at08.00.58.png" alt="Screenshot 2020-10-24 at 08.00.58" style="zoom:50%;" />

Si je mets l'`attribute` deux fois :

```csharp
[Display("First Name :", System.ConsoleColor.Cyan)]
[Indent]
[Indent]
public string FirstName { get; set; }
```

J'obtiens deux indentation :

<img src="assets/Screenshot2020-10-24at08.02.14.png" alt="Screenshot 2020-10-24 at 08.02.14" style="zoom:50%;" />
