# 01 `Binding`

## `Data Binding`  :  `@bind`

`@bind` fonctionne dans les deux sens : `two way data binding`.

La mise à jour de la valeur se fait à la perte du focus (`onchange`) et non quand la valeur change (`oninput`). 

Dans un formulaire lie un champ avec le `Model` :

```cs
<select name="Type" id="Type" @bind="robotModel.RobotType">
  <option value="0" disabled selected>-- Select Type --</option>
  @foreach (var robotType in Enum.GetValues<RobotType>())
  {
    <option value="@robotType">@robotType</option>
  }
</select>
```

`Enum.GetValues<ModelType>()` renvois un tableau des constantes de l'énumération.

dans le `template` un type `Enum` est directement converti en texte.

### Ajouter `bind` sur un autre événement : `@bind:event`

```cs
<label for="Name">Name : @robotModel.Name</label>
        <input type="text" id="Name" 
  				@bind="robotModel.Name" 
  				@bind:event="oninput">
```

On retrouve le `two way binding` classique d'autre `framework`.

### Avec un `checkbox`

```cs
<input type="checkbox" @bind="checkDelete" />
  
@code {
  bool checkDelete;
}
```



## `bind-...`

`...` correspond à la propriété sur laquelle le `binding` s'applique.
`@bind-value` est un alias de `@bind`.

Par contre avec un `InputText`, c'est un `V` majuscule car c'est la propriété d'un composant `Blazor` et non l'attribut `HTML` `value="..."` : `@bind-Value`.

