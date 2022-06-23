# 07 Appel asynchrone

## Utilisation des `Task<>`

Dans `ICharacterService.cs` on va entourer nos type avec `Task<>` :

```csharp
using System.Collections.Generic;
using System.Threading.Tasks;
using dotnet_rpg.Models;

namespace dotnet_rpg.Services.CharacterService
{
    public interface ICharacterService
    {
        Task<List<Character>> GetAllCharacters();

        Task<Character> GetCharacterById(int id);
        Task<List<Character>> AddCharacter(Character newCharacter);
    }
}
```

On utilise `using System.Threading.Tasks`.

On modifie aussi `CharacterService.cs` de la même manière en ajoutant le mot clé `async` :

```csharp
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using dotnet_rpg.Models;

namespace dotnet_rpg.Services.CharacterService
{
    public class CharacterService : ICharacterService
    {
        public List<Character> characters = new()
        {
            new(),
            new() { Id = 1, Name = "Sam" }
        };

        public async Task<List<Character>> AddCharacter(Character newCharacter)
        {
            characters.Add(newCharacter);
            return characters;
        }

        public async Task<List<Character>> GetAllCharacters()
        {
            return characters;
        }

        public async Task<Character> GetCharacterById(int id)
        {
            return characters.FirstOrDefault(c => c.Id == id);
        }
    }
}
```

De même on va modifier `CharacterController.cs` et on ajoute le mot clé `await` devant l'appelle des méthodes asynchrone de `CharacterService.cs` :

```csharp
using // ...

namespace dotnet_rpg.Controllers
{

    [ApiController]
    [Route("[controller]")]
    public class CharacterController : ControllerBase
    {
        private readonly ICharacterService _characterService;
        public CharacterController(ICharacterService characterService)
        {
            _characterService = characterService;
        }

        [HttpGet("AllCharacters")]
        public async Task<ActionResult<List<Character>>> GetAllCharacters()
            => Ok(await _characterService.GetAllCharacters());


        [HttpGet("{id}")]
        public async Task<ActionResult<Character>> Get(int id)
        {
            var character = await _characterService.GetCharacterById(id);

            if (character is null)
            {
                Console.WriteLine("character is null");
                return NotFound();
            }

            return Ok(character);
        }

        [HttpPost]
        public async Task<ActionResult<List<Character>>> AddCharacter(Character newCharacter)
            => Ok(await _characterService.AddCharacter(newCharacter));

    }
}
```

