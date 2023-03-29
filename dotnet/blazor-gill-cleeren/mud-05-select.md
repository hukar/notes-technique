# Mud-05 `MudSelect`

## Mise en place

Pour qu'une classe puisse être utilisée dans un `MudSelect` comme `type` de donnée, il faut `overrider` 3 méthodes : `Equals`, `GetHashCode` et `ToString` (pour l'affichage du texte dans le `select`).

```cs
namespace Domain;

public class Weapon
{
    public int Id { get; set; }
    public string Name { get; set; } = string.Empty;
    public int RobotId { get; set; }
    
    // Note: c'est important pour que MudSelect puisse comparer les Weapons
    public override bool Equals(object? o)
    {
        var other = o as Weapon;
        return other?.Name == Name;
    }

    // Note: c'est important aussi !
    public override int GetHashCode() => Name?.GetHashCode() ?? 0;
    
    // Implémenter pour que les Weapons s'affiche correctement dans le MudSelect
    public override string ToString() => Name;
}
```



## Simple `MuSelect` : avec image

```ruby
<MudSelect 
    Label="Favourite Weapons" 
    @bind-Value="@Robot.FavouriteWeapon">
    @foreach (var weapon in _weapons)
    {
        <MudSelectItem Value="@weapon">
            <img src="@($"images/weapon-{weapon.Id}.png")" height="14" class="mr-1" /> @weapon.Name
        </MudSelectItem>
    }
</MudSelect>
            
@code {
    [Inject]
    public IRobotRepository Repo { get; set; } = default!;
    
    public Robot Robot { get; set; } = new();
    
    IEnumerable<Weapon> _weapons = default!;
    
    protected override async Task OnInitializedAsync()
    {
        _weapons = await Repo.GetAllWeapons();
    }
```

<img src="assets/mudselect-simple-in-action.png" alt="mudselect-simple-in-action" />

On peut ajouter un attribut `AnchorOrigin` pour déterminer comment apparait le `Pop-Over`:

```ruby
<MudSelect 
    Label="Favourite Weapons" 
    @bind-Value="@Robot.FavouriteWeapon"
    AnchorOrigin="Origin.BottomCenter">
```

<img src="assets/set-origin-pop-over.png" alt="set-origin-pop-over" />



## `MultiSelection` avec `MudSelect`

`MultiSelection="true"`

```ruby
<MudSelect
    Label="List Secondary Weapons" 
    MultiSelection="true"
    @bind-SelectedValues="@Robot.Weapons">
    @foreach (var weapon in _weapons)
    {
        <MudSelectItem Value="@weapon">
            <img src="@($"images/weapon-{weapon.Id}.png")" height="14" class="mr-1" /> @weapon.Name
        </MudSelectItem>
    }
</MudSelect>
```

Dans la classe `Robot.cs`

```cs
public class Robot
{
    // ...
    public IEnumerable<Weapon> Weapons { get; set; } = new HashSet<Weapon>();
    // ...
```

`Weapons` doit **obligatoirement** être déclaré comme `IEnumerable`.

On utilise un `HashSet` pour ne pas avoir deux éléments identiques, mais cela peut très bien fonctionner avec une `List<T>`:

```cs
public IEnumerable<Weapon> Weapons { get; set; } = new List<Weapon>();
```

Cela peut même fonctionner sans instance directe de l'`IEnumerable`:

```cs
 public IEnumerable<Weapon> Weapons { get; set; } // = new List<Weapon>();
```

Il faut alors mettre une condition dans le `template`:

```ruby
<MudPaper Elevation="2" Class="pa-8 mt-4">
    @if (Robot.Weapons is not null)
    {
        @foreach (var weapon in Robot.Weapons)
        {
            <MudText Typo="Typo.body1">@weapon.Name</MudText>
        }
    }
</MudPaper>
```

Et cela fonctionne:

<img src="assets/multi-select-without-instance-of-collection.png" alt="multi-select-without-instance-of-collection" />

Ce n'est peut-être malgré tout pas une bonne pratique de laisser un `IEnumerable` non initialisé.







