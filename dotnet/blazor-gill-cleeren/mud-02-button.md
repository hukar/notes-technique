## Mud-02 `MudButton`



## Couleur de l'icône

On peut régler la couleur de l'icône uniquement grâce à la propriété `IconClass` et au classe de couleur `material` de `MudBlazor`:

```ruby
<MudButton 
	StartIcon="@Icons.Material.Filled.Face5" 
	IconClass="pink-text text-accent-3" 
	Variant="Variant.Text" 
	Color="Color.Primary">Employee Details</MudButton>
```

<img src="assets/employee-details-button-icon-color-custom.png" alt="employee-details-button-icon-color-custom" />



## Élévation du bouton: `DisableElevation="true"`

```ruby
<MudButton 
		 Variant="Variant.Filled" 
		 Color="Color.Primary">Button One</MudButton>
	 <MudButton 
		 Variant="Variant.Filled" 
		 Color="Color.Primary" 
		 DisableElevation="true">Button Two</MudButton>
```

<img src="assets/button-elevation-disable-or-not.png" alt="button-elevation-disable-or-not" />