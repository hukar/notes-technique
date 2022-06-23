# 05 la méthode `HTTP` :  `GET`



## `OnGet`

Dans la fiche `04` on récupère l'heure directement dans une propriété.

On pourrait ne la récupérer que lorsque la page est appelée en `GET` :

```cs
public class MyPage : PageModel
{
    public DateTime date;
    
    public void OnGet()
    {
        date = DateTime.Today;
    }
}
```

Il existe un pendant asynchrone à cette méthode si besoin :

```cs
public async Task OnGetAsync()
{
    _context.Records.ToListAsync();
}
```

On ne peut pas utiliser les deux méthodes à la foi.