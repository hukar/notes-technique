# 04 Initialiser les données de la `BDD`

Dans `Data` on crée une classe `AppDbInitialer` 

```cs
using System;
using System.Linq;
using Microsoft.AspNetCore.Builder;
using Microsoft.Extensions.DependencyInjection;
using my_books.Data.Models;

namespace my_books.Data
{
  public class AppDbInitialer
  {
    public static void Seed(IApplicationBuilder applicationBuilder)
    {
      using (var servicesScope = applicationBuilder.ApplicationServices.CreateScope())
      {
        var context = servicesScope.ServiceProvider.GetService<AppDbContext>();

        if (context.Books.Any())
        {
          context.Books.AddRange(new Book(){/* ... */}, new Book(){/* ... */});

          context.SaveChanges();
        }
      }
    }
  }
}
```

On appelle ensuite cette méthode dans 