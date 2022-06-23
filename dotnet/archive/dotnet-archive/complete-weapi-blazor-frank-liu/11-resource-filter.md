# `ResourceFilter`

Le `ResourceFilter` se produit abvant le `Model Binding`.



## Création d'un `ResourceFilter`

> **scénario** :
>
> On arrête la `version 1` de notre `api`.

Dans le dossier `Filters`, on va créer une nouvelle classe `Version1DiscontinueResourceFilter.cs`.

Dérive de la classe `Attribute` et implémente l'interface `IResourceFilter`.

```cs
using System;
using Microsoft.AspNetCore.Mvc.Filters;

namespace Filters
{
    public class Version1DiscontinueResourceFilter : Attribute, IResourceFilter
    {
        public void OnResourceExecuted(ResourceExecutedContext context)
        {
            // throw new NotImplementedException(); 
            // on n'a pas besoin de cette méthode
        }

        public void OnResourceExecuting(ResourceExecutingContext context)
        {
            if (!context.HttpContext.Request.Path.Value.ToLower().Contains("v2"))
            {
                // si ce n'est pas la v2 on court-circuite
                context.Result = new BadRequestObjetResult(
                	new
                    {
                        Versionning = new[] {"this version of the API has expired, please use the latest version"}
                    }
                );
            }
        }
    }
}
```

Pour court-circuiter le `pipeline`, il faut que le context renvoie un `Result` avec `context.Result =`.

On peut appliquer ce filtre au contrôleur :

`TicketController.cs`

```cs
[ApiController]
[Route("api/[controller]")]
[Version1DiscontinueResourceFilter]
public class TicketsController : ControllerBase
{
```

On peut aussi appliquer le `filter` de manière globale (pour tous les contrôleur).

Dans `Startup.cs` on ajoute cela à `AddController` :

```cs
public void ConfigureServices(IServiceCollection services)
{
    // services.AddControllers(); <= avant
    services.AddController(options => {
        options.Filters.Add<Version1DiscontinueResourceFilter>();
    });
```

On peut maintenant le retirer du contrôleur.



## Exercice

On veut un filtre qui vérifie que la `EntereredDate` est antérieur ou égal à la `DueDate`.

On va ré-utiliser notre `ActionFilter` : `Ticket_EnsureEntereddate`.

On va le renommer en `Ticket_ValidateDatesActionFilter.cs`.

```cs
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.Filters;
using Models;

namespace Filters
{
    public class Ticket_EnsureDatesActionFilter : ActionFilterAttribute
    {
        public override void OnActionExecuting(ActionExecutingContext context)
        {
            base.OnActionExecuting(context);

            var ticket = context.ActionArguments["ticket"] as Ticket;

            if (ticket is not null && !string.IsNullOrWhiteSpace(ticket.Owner)) 
            {
                var isValid = true;
                if (!ticket.EnteredDate.HasValue)
                {
                    context.ModelState.AddModelError("EnteredDate", "EnteredDate is required");
                    isValid = false;
                }
                if(ticket.EnteredDate.HasValue && ticket.DueDate.HasValue && ticket.EnteredDate > ticket.DueDate)
                {
                    context.ModelState.AddModelError("DueDate", "DueDate has to be later than  the EnteredDate");
                    isValid = false
                }
                
                if(!isValid)
                {
                    // short-circuit
                	context.Result = new BadRequestObjectResult(context.ModelState);
                }
            }
        }
    }
}
```

