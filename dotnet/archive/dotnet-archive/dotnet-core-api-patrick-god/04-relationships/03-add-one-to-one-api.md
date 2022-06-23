# Ajouter la relation `one-to-one` à l'`API`



## Création de `WeaponService`

Dans le dossier `Services` créer un sous-dossier `WeaponService` et dedans `IWeaponService.cs` et `WeaponService.cs`

### `IWeaponService.cs`

```cs
using System.Threading.Tasks;
using dotnet_rpg.Dtos.Character;
using dotnet_rpg.Dtos.Weapon;
using dotnet_rpg.Models;

namespace dotnet_rpg.Services.WeaponService
{
    public interface IWeaponService
    {
        Task<ServiceResponse<GetCharacterDto>> AddWeapon(AddWeaponDto newWeapon);
    }
}
```

Il faut un nouveau `DTO`.

dans `Dtos` on crée un dossier `Weapon`et dedans une nouvelle class `AddWeaponDto.cs`

```cs
public class AddWeaponDto
{
    public string Name { get; set; }
    public int Damage { get; set; }
    public int characterId { get; set; }
}
```





### `WeaponService.cs`

On a besoin du constructeur pour injecter `DataContext` et `HttpContextAccessor` pour récupérer le `User`.

On injecte aussi `AutoMapper`.

La méthode `AddWeapon` est `async`.

```cs
using System.Threading.Tasks;
using AutoMapper;
using dotnet_rpg.Data;
using dotnet_rpg.Dtos.Character;
using dotnet_rpg.Dtos.Weapon;
using dotnet_rpg.Models;
using Microsoft.AspNetCore.Http;

namespace dotnet_rpg.Services.WeaponService
{
    public class WeaponService : IWeaponService
    {
        private readonly DataContext _context;
        private readonly HttpContextAccessor _httpContextAccessor;
        private readonly IMapper _mapper;
        public WeaponService(DataContext context, HttpContextAccessor httpContextAccessor, IMapper mapper)
        {
            _mapper = mapper;
            _httpContextAccessor = httpContextAccessor;
            _context = context;

        }
        public async Task<ServiceResponse<GetCharacterDto>> AddWeapon(AddWeaponDto newWeapon)
        {
            throw new System.NotImplementedException();
        }
    }
}
```

### Ajout de la propriété `GetUserId`

```cs
private int GetUserId => int.Parse(_httpContextAccessor.HttpContext.User.FindFirstValue(ClaimTypes.NameIdentifier));
```



### Implémentation de `AddWeapon`

1. Initialiser `ServiceResponse`
2. Retourner le `ServiceResponse`
3. Créer un `try/catch` pour remplir correctement le `ServiceResponse`



```cs
public async Task<ServiceResponse<GetCharacterDto>> AddWeapon(AddWeaponDto newWeapon)
{
    ServiceResponse<GetCharacterDto> response = new();

    try
    {
        var character = await _context.Characters
            .FirstOrDefaultAsync(c => c.Id == newWeapon.characterId && c.User.Id == GetUserId);

        if (character is null)
        {
            response.Success = false;
            response.Message = "Character not found";

            return response;
        }

        var weapon = new Weapon
        {
            Name = newWeapon.Name,
            Damage = newWeapon.Damage,
            Character = character
        }
        
        _context.Weapons.Add(weapon);
        await _context.SaveChangesAsync();
        
        response.Data = _mapper.Map<GetCharacterDto>(character);
    }
    catch (Exception ex)
    {

        response.Success = false;
        response.Message = ex.Message;
    }

    return response;
}
```

On enregistre la nouvelle `weapon` grace à :

```cs
_context.Weapons.Add(weapon);
await _context.SaveChangesAsync();
```

On utilise le `_mapper` pour renvoyer un `GetCharacterDto` depuis `character`.



## Création du contrôleur `Controller/Weapon.cs`

```cs
namespace dotnet_rpg.Controllers
{
    public class WeaponController
    {

    }
} 
```

