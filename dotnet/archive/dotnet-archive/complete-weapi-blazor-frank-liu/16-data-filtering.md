# 16 Data `Filtering`

On va créer un dossier `QueryFilters` dans le projet `WebApi` et dedans une classe `TicketQueryFilter.cs`



## `TicketQueryFilter`

```cs
namespace WebApi.QueryFilters
{
    public class TicketQueryFilter
    {
        public int? Id { get; set; }
        public string Title { get; set; }
        public string Description { get; set; }
    }
}
```





## dans `TicketsController`

On va modifier la méthode `GET` :

```cs
[HttpGet]
public async Task<IActionResult> Get([FromQuery] TicketQueryFilter ticketQueryFilter)
{
    IQueryable<Ticket> tickets = _db.Tickets;
    
    if (ticketQueryFilter is not null)
    {
        if (ticketQueryFilter.Id.HasValue)
        {
            tickets = tickets.Where(t => t.TicketId == ticketQueryFilter.Id);
        }
        
        if(!string.IsNullOrWhiteSpace(ticketQueryFilter.Title))
        {
            tickets = tickets.Where(t => t.Title.Contains(ticketQueryFilter.Title, StringComparaison.OrdinalIgnoreCase))
        }
        
        if(!string.IsNullOrWhiteSpace(ticketQueryFilter.Description))
        {
            tickets = tickets.Where(t => t.Description.Contains(ticketQueryFilter.description, StringComparaison.OrdinalIgnoreCase))
        }
    }
    
    
    return Ok(await tickets.ToListAsync());
}
```

`DbSet` implémente l'interface `IQueryable`. 

On peut ajouter des clauses `Where` ou `Skip` qui seront appliquées lors de l'appelle de `ToListAsync`.

```cs
IQueryable<Ticket> tickets = _db.Tickets;

tickets = tickets.Where(t => t.TicketId == ticketQueryFilter.Id);
tickets = tickets.Skip(5);

await _db.Tickets.ToListAsync() // <= appliqués ici
```

<img src="assets/query-filter-works-good.png" alt="query-filter-works-good" style="zoom:50%;" />

Cela fonctionne bien et on peut voire l'`url` de requête dans `Swagger`.



## `OData`

Voire le nuget package `Odata`.

https://docs.microsoft.com/fr-fr/odata/

Fait la même chose que nos filtres si des besoins complexes.

Pas encore de version pour `.net 5` à suivre ...















