# 02. `Attributes` et Debug

## Debug `Attributes`

### Afficher des infos custom `[DebuggerDisplay("message")]`

<img src="assets/Screenshot2020-10-22at11.48.01.png" alt="Screenshot 2020-10-22 at 11.48.01" style="zoom:50%;" />

<img src="assets/Screenshot2020-10-22at11.50.57.png" alt="Screenshot 2020-10-22 at 11.50.57" style="zoom:50%;" />

On voit lors du Debug les métadonnées affichées au survol ou dans l'explorateur : `{csharp_attributes.Contact}`.

On va pouvoir personnaliser ces métadonnées.

Dans notre classe `Contact.cs` :

```csharp
using System.Diagnostics;

namespace csharp_attributes
{
    [DebuggerDisplay("Fisrt name = {FirstName} Age In Years = {AgeInYears}")]
    public class Contact
    {
        public string FirstName { get; set; }
        public int AgeInYears { get; set; }
    }
}
```

<img src="assets/Screenshot2020-10-22at11.53.25.png" alt="Screenshot 2020-10-22 at 11.53.25" style="zoom:50%;" />

### Ne pas afficher la valeur d'une propriété dans le debugger

### `[DebuggerBrowsable(option)]`

```csharp
public class Contact
{
    public string FirstName { get; set; }

    [DebuggerBrowsable(DebuggerBrowsableState.Never)]
    public int AgeInYears { get; set; }
}
```

<img src="assets/Screenshot2020-10-22at12.02.55.png" alt="Screenshot 2020-10-22 at 12.02.55" style="zoom:50%;" />

L'`AgeInYears` n'apparait plus dans la boîte de dialogue du `Debugger`.

### Utilisation d'un `proxy debug` : `[DebuggerTypeProxy(Type t)]`

On peut créer une classe qui va jouer le rôle d'un proxy de Debug, c'est à dire fournir des informations calculées sur la classe.

classe de `proxy` : `ContactDebugDisplay.cs`

```csharp
namespace csharp_attributes
{
    internal class ContactDebugDisplay
    {
        private readonly Contact _contact;

        public ContactDebugDisplay(Contact contact)
        {
            _contact = contact;
        }

        public string UpperName =>  _contact.FirstName.ToUpperInvariant();
        public string AgeInHex => _contact.AgeInYears.ToString("X");
    }
}
```

`ToString("X")` pour afficher la valeur décimal en `hexa`.

Dans `Contact.cs`

```csharp
using System.Diagnostics;

namespace csharp_attributes
{
    [DebuggerDisplay("Fisrt name = {FirstName} Age In Years = {AgeInYears}")]
    [DebuggerTypeProxy(typeof(ContactDebugDisplay))]
    public class Contact
    {
        public string FirstName { get; set; }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int AgeInYears { get; set; }
    }
}
```

<img src="assets/Screenshot2020-10-22at12.17.49.png" alt="Screenshot 2020-10-22 at 12.17.49" style="zoom:50%;" />

On voit que nos infos issues du `Proxy` de Debug apparaissent bien.
