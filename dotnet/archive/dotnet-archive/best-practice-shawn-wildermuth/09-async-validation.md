# 09 Asynchronous `Validation`

## Comment garantir que le `Name` est unique ? `MustAsync`

```cs
public ClientViewModelValidator(BillingContext ctx)
{
  RuleFor(c => c.Name)
    .NotEmpty()
    .MaximumLengh(100)
    .MinimumLength(3)
    .MustAsync(async (model, value, cancellationToken) => {
			return !(await ctx.Clients.AnyAsync(c => c.Name == value && model.Id != c.Id));
    })
    .WithMessage("Name must be unique");

```

`MustAsync` prend une `async lambda` avec trois paramètres : le `model`, la `value` et le `cancellation token`.

On doit aussi passer le `context` au constructeur `BillingContext ctx`.

Retourne `false` s'il y a au moins un élément qui a le même nom avec une `Id` différente.

<img src="assets/async-validation-unique-name.png" alt="async-validation-unique-name" style="zoom:50%;" />

