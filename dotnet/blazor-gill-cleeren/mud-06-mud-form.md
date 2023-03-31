# Mud 06 `MudForm`

> Pour un `MudForm`, on ne doit pas utiliser un bouton avec `ButtonType="ButtonType.Submit"`



## Validation Simple: `Required` et `RequiredError`

```cs
<MudForm @ref="form" @bind-IsValid="success" @bind-Errors="errors">
	<MudTextField 
    	T="string" 
    	Label="Username" 
    	Required="true" 
    	RequiredError="User name is required!"/>
    
    <MudButton Disabled="@(!success)">Register</MudButton>
</MudForm>
```

```cs
bool success;
string[] errors = { };

MudForm form;
```

<img src="assets/simple-field-validation-work-simple.png" alt="simple-field-validation-work-simple" />



## Déclencher la validation programmatiquement

On va ajouter trois `Button` pour contrôler notre `Form`:

### `Validate Form`: `form.Validate()`

```ruby
<MudButton OnClick="@(()=>form.Validate())">Validate</MudButton>
```



### `Reset Form`: `form.Reset()`

```cs
<MudButton OnClick="@(()=>form.Reset())" Class="mx-2">Reset</MudButton>
```

`success` reste ou passe à `false`, les champs sont vidés et `errors` est vidé aussi.

<img src="assets/reset-form-button-action-consequence.png" alt="reset-form-button-action-consequence" />



### `Reset Validation`: `form.ResetValidation()`

```ruby
<MudButton OnClick="@(()=>form.ResetValidation())">Reset Validation</MudButton>
```

Vide `errors` et met `success` à `true`.

<img src="assets/reset-validation-effect-button.png" alt="reset-validation-effect-button" />



## `Helper` pour afficher la validation

```ruby
<MudPaper Elevation="2" Class="mt-4 pa-8">
    <MudText Typo="Typo.body1" Color="@(_success ? Color.Success : Color.Error)">
        Success: @_success
    </MudText>
    @foreach (var error in _errors)
    {
        <MudText Typo="Typo.body1" Color="Color.Error">
            @error
        </MudText>
    }
</MudPaper>
```

<img src="assets/bouya-validation-helper.png" alt="bouya-validation-helper" />



## Validation d'une adresse `mail` avec la propriété `Validation` et les attributs `DataAnnotations`

 Peut recevoir un attribut de validation (notre exemple) ou une fonction de validation.

Les attributs de validation sont fournis par la librairie `System.ComponentModel.DataAnnotations`.

```html
@using System.ComponentModel.DataAnnotations
    
<MudForm @ref="form" @bind-IsValid="success" @bind-Errors="errors">
	<MudTextField 
		T="string" 
		Label="Email"
		Validation="@(new EmailAddressAttribute() {
				ErrorMessage = "The email address is invalid"
		})"/>
</MudForm>
```

### Ici les attributs de validation sont passés sous forme d'une instance de `System.ComponentModel.DataAnnotations.ValidationAttribute` :  `new MyValidationAttribute()`.

<img src="assets/email-validation-attribute-instance.png" alt="email-validation-attribute-instance" />



## Validation d'un `password` en passant une `Func` à la propriété `Validation`

Voici la listes des `Func` acceptées:

Les types pris en charge sont:
- `Func<T, bool>` ... affichera le message d'erreur standard "Invalid" s'il est `false`
- `Func<T, string>` ... affiche le résultat en tant que message d'erreur, pas d'erreur si `null`
- `Func<T, IEnumerable<string>>` ... affiche tous les messages d'erreur retournés, pas d'erreur s'il est vide
- `Func<object, string, IEnumerable<string>>`  input de `Form.Model`, `Full Path` du `Member` ... affiche tous les messages d'erreur retournés, pas d'erreur s'il est vide

Et les versions `async`:

- `Func<T, Task<bool>>` 
- `Func<T, Task<string>>` 
- `Func<T, Task<IEnumerable<string>>>` 
- `Func<object, string, Task<IEnumerable<string>>>`

```html
<MudTextField 
    T="string" 
    Label="Password" 
    HelperText="Choose a strong password" 
    @ref="pwField1"
    InputType="InputType.Password"
    Validation="@(new Func<string, IEnumerable<string>>(PasswordStrength))" />
<MudTextField 
    T="string"
    Label="Password" 
    HelperText="Repeat the password" 
    InputType="InputType.Password"
    Validation="@(new Func<string, string>(PasswordMatch))" />
```

On créé une référence sur le `TextField` avec `@ref="pwField1"`.

Pour utiliser les `expressions régulières` on doit ajouter ce `using`:

```ruby
@using System.Text.RegularExpressions
```

On a ici `Func<T, IEnumerable<string>>` et ensuite `Func<T, string>` avec `T=string`.

```cs
@code {
    // ...
    MudTextField<string> pwField1;

    private IEnumerable<string> PasswordStrength(string pw)
    {
        if (string.IsNullOrWhiteSpace(pw))
        {
            yield return "Password is required!";
            yield break;
        }
        if (pw.Length < 8)
            yield return "Password must be at least of length 8";
        if (!Regex.IsMatch(pw, @"[A-Z]"))
            yield return "Password must contain at least one capital letter";
        if (!Regex.IsMatch(pw, @"[a-z]"))
            yield return "Password must contain at least one lowercase letter";
        if (!Regex.IsMatch(pw, @"[0-9]"))
            yield return "Password must contain at least one digit";
    }

    private string PasswordMatch(string arg)
    {
        if (pwField1.Value != arg)
            return "Passwords don't match";
        return null;
    }
```



### Simplification de la syntaxe

On peut simplifier :

```ruby
<MudTextField 
    ...
    Validation="@(new Func<string, IEnumerable<string>>(PasswordStrength))" />
```

en

```ruby
<MudTextField
        ...
        Validation="PasswordStrength"/>
```

On a alors un `Warning`:

<img src="assets/warning-func-simple-syntax.png" alt="warning-func-simple-syntax" />

On peut désactiver ce `warning` dans le fichier `.csproj`:

```xml
	...
	<PropertyGroup>
    	<NoWarn>8974</NoWarn>
  	</PropertyGroup>

</Project>
```





## Exemple simple avec `FluentValidation` : `ccValidator`

```ruby
@using FluentValidation

<MudForm>
    <MudTextField 
        @bind-Value="creditCardNr" 
        Validation="@ccValidator.Validation" 
        Immediate="true" 
        Label="Credit card nr" />
</MudForm>
    
<MudText>credit card number: @_creditCardNr</MudText>
```

`Immediate`: Si `true` l'élément `input` met à jour la `Value` immédiatement lorsqu'on tape. Si c'est à `false`, la `Value` est mise à jour uniquement si on tape `Enter`.

<img src="assets/immediate-true-or-false.png" alt="immediate-true-or-false" />



### Version simple

<img src="assets/fluent-validation-mud-blazor-simple-cc.png" alt="fluent-validation-mud-blazor-simple-cc" />

```ruby
<MudForm @ref="_form" @bind-IsValid="_success" @bind-Errors="_errors">
    <MudTextField
        @bind-Value="_creditCardNr" 
        Validation="ValidateCreditCard" 
        Immediate="true" 
        Label="Credit card nr" />
</MudForm>
```

```cs
@code {
    // ...

    // This is a valid Visa test card number
    private string _creditCardNr = "4012 8888 8888 1881";

    CreditCardValidator ccValidator = new CreditCardValidator();

    public IEnumerable<string> ValidateCreditCard(string creditCard)
    {
        var result = ccValidator.Validate(creditCard);

        if (result.IsValid) return new string[] { };

        return result.Errors.Select(error => error.ErrorMessage);
    }
}
```

Et le `Validator`:

```cs
public class CreditCardValidator : AbstractValidator<string>
{
    public CreditCardValidator()
    {
        RuleFor(cc => cc)
            .NotEmpty().WithMessage("Ne soit pas null Credit Card ni vide")
            .Length(1, 100)
            .CreditCard().WithMessage("Numero de carte invalide");
    }
    
}
```

Il suffit que la méthode de validation respecte les signatures authorisées pour la propriété `Validation`, ici `Func<string, IEnumerable<string>>`.

















