# 06. Code Asynchrone

Pour ne pas bloquer son `API`, on doit utiliser des méthodes en asynchrone.

Dans `Controleers/UsersController.cs`

```csharp
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
// ...

// api/users
[HttpGet]
public async Task<ActionResult<IEnumerable<AppUser>>> GetUsers() => await _context.Users.ToListAsync();

// api/users/3
[HttpGet("{id}")]
public async Task<ActionResult<AppUser>> GetUser(int id) => await _context.Users.FindAsync(id);
```

`async` spécifie que la méthode est asynchrone.

`Task` permet de renvoyer une tâche asynchrone, on utilise `using System.Threading.Tasks;`

`await` demande d'attendre la réponse avant de la renvoyer.

`ToListAsync` et `FindAsync` utilise la `DB` de manière asynchrone, il font partie de `Microsoft.EntityFrameworkCore`.

`using System.Linq` n'est plus utile.

