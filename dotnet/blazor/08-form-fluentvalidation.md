# 08 Validation de formulaire avec `FluentValidation`

## Modification du modèle

On n'utilise plus ici les `annotations` pour la `validation`, mais le package `FluentValidation`.

```bash
dotnet add package fluentvalidation
```



```cs
using FluentValidation;

public class RobotModel
{
  public string Name { get; set; }
  public string Description { get; set; }
  public string RobotType { get; set; }
  public int Speed { get; set; }
}

public class RobotModelValidator : AbstractValidator<RobotModel>
{
  public RobotModelValidator()
  {
    RuleFor(r => r.Name)
      .NotEmpty().WithMessage("{PropertyName} can not be empty like this")
      .MinimumLength(5).WithMessage("{PropertyName} must have more characters than {ComparisonValue}");

    RuleFor(r => r.RobotType)
      .NotEmpty().WithMessage("{PropertyName} can not be empty like that");

    RuleFor(r => r.Speed)
      .GreaterThanOrEqualTo(2).WithMessage("{PropertyName} must be greater or equal to {comparaisonValue}");
  }
}
```



## Installation : `Blazored.FluentValidation`

```bash
dotnet add package Blazored.FluentValidation
```

#### Dans `_Imports.razor` on ajoute :

```cs
@using Blazored.FluentValidation
```



On peut maintenant remplacer `<DataAnnotationsValidator />` par `<FluentValidationValidator />` :

```html
<EditForm Model="@robotModel" OnValidSubmit="HandleFormSubmit" OnInvalidSubmit="HandleFormInvalid">
  <FluentValidationValidator />
  <ValidationSummary />
```



## Avec `MudBlazor`

```cs
<EditForm Model="model" OnValidSubmit="OnValidSubmit">
  <FluentValidationValidator />
  <ValidationSummary />
  
  <MudTextField 
  	Label="First name" 
  	HelperText="Max. 16 characters"
    @bind-Value="model.Name" 
    For="@(() => model.Name)" />

  <MudButton 
  	ButtonType="ButtonType.Submit" 
  	Variant="Variant.Filled" 
  	Color="Color.Primary" 
  	Class="ml-auto"
  	Disabled="@(!context.IsModified() || !context.Validate())">
  		Register</MudButton>
```

On a `For="@(() => model.Name)"` qui permet de définir la propriété devant être validé.

`@bind-Value="model.Name"` relie la valeur introduite dans le composant `MudTextField` à l'objet servant de `model` au formulaire.

La validation classique elle se présente comme ça :

```cs
<ValidationMessage For="() => robotModel.Name" />
```

et une classe `invalid` est automatiquement ajoutée.

Avec `MudBlazor` on a une classe `mud-input-error` qui est ajoutée.
