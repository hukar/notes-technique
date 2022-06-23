# 04 Implémentation des services

On peut retirer la liste statique :

```cs
public static List<Character> characters = new()
{
    new(),
    new() { Id = 1, Name = "Sam" }
};
```

`static` car on utilise `AddScoped` à la place de `AddSingleton` et si elle n'était pas statique, elle serait réinitialisée à chaque instance du service (à chaque requête avec `AddScoped`).

## Modification de `CharacterService.cs`

On va injecter notre service `DataContext` dans le constructeur de `CharacterService`.

```cs
private readonly IMapper _mapper;
private readonly DataContext _context;

public CharacterService(IMapper mapper, DataContext context)
{
    _context = context;
    _mapper = mapper;
}
```



## Modification de `GetAllCharacters`

On va checher les `Character` en `BDD` plutôt qu'en `RAM`.

```cs
public async Task<ServiceResponse<List<GetCharacterDto>>> GetAllCharacters()
{
    ServiceResponse<List<GetCharacterDto>> serviceResponse = new();

    var dbCharacters = await _context.Characters.ToListAsync();

    serviceResponse.Data = dbCharacters.Select(c => _mapper.Map<GetCharacterDto>(c)).ToList();

    return serviceResponse;
}
```

`ToListAsync` est une méthode d'`entity framework core`.

#### ! `DataContext` ne peut être utilisé avec `AddSingleton`, il faut utiliser `AddScoped`.

```cs
// Startup.cs

// services.AddSingleton<ICharacterService, CharacterService>();
services.AddScoped<ICharacterService, CharacterService>();
```



## Modification `GetCharacterById`

```cs
var dbCharacter = await _context.characters.FirstOrDefaultAsync(c => c.Id == id);
```

```cs
public async Task<ServiceResponse<GetCharacterDto>> GetCharacterById(int id)
{
    ServiceResponse<GetCharacterDto> serviceResponse = new();

    var dbCharacter = await _context.Characters.FirstOrDefaultAsync(c => c.Id == id);

    serviceResponse.Data = _mapper.Map<GetCharacterDto>(dbCharacter);

    return serviceResponse;
}
```

Maintenant qu'on utilise les méthodes `Async` avec `await`, les `warning` disparaissent.



## Modification `AddCharacter`

On veut que `sql server` ajoute l'`id` automatiquement et ne plus le faire manuellement.

`_context.Characters.Add(character)` n'a pas besoin d'être `async` car à ce stade, le nouveau `character` n'est pas inséré dans la base de données.

#### ! Pour insérer le `character` dans la `bdd` il faut utiliser `SaveChangesAsync`.

```cs
await _context.SaveChangesAsync();
```

Ensuite on renvoie tous les `characters` de la base de données :

```cs
serviceResponse.Data = await _context
    .Characters.Select(c => _mapper.Map<GetCharacterDto>(c))
    .ToListAsync();
```

### `AddCharacter(AddCharacterDto newCharacter)`

```cs
public async Task<ServiceResponse<List<GetCharacterDto>>> AddCharacter(AddCharacterDto newCharacter)
{
    ServiceResponse<List<GetCharacterDto>> serviceResponse = new();

    var character = _mapper.Map<Character>(newCharacter);

    _context.Characters.Add(character);

    await _context.SaveChangesAsync();

    serviceResponse.Data = await _context.Characters
        .Select(c => _mapper.Map<GetCharacterDto>(c))
        .ToListAsync();

    return serviceResponse;
}
```



## Modification `UpdateCharacter`

```cs
public async Task<ServiceResponse<GetCharacterDto>> UpdateCharacter(UpdateCharacterDto updatedCharacter)
{
    ServiceResponse<GetCharacterDto> serviceResponse = new();

    try
    {
        var character = await _context.Characters.FirstOrDefaultAsync(c => c.Id == updatedCharacter.Id);

        character.Name = updatedCharacter.Name;
        character.HitPoints = updatedCharacter.HitPoints;
        character.Strength = updatedCharacter.Strength;
        character.Defense = updatedCharacter.Defense;
        character.Intelligence = updatedCharacter.Intelligence;
        character.Class = updatedCharacter.Class;

        await _context.SaveChangesAsync();

        serviceResponse.Data = _mapper.Map<GetCharacterDto>(character);
    }
    catch (Exception ex)
    {

        serviceResponse.Success = false;
        serviceResponse.Message = ex.Message;
    }

    return serviceResponse;
}
```



## Modification de `DeleteCharacter`

```cs
public async Task<ServiceResponse<List<GetCharacterDto>>> DeleteCharacter(int id)
{
    ServiceResponse<List<GetCharacterDto>> serviceResponse = new();

    try
    {
        var character = await _context.Characters.FirstAsync(c => c.Id == id);

        _context.Characters.Remove(character);

        await _context.SaveChangesAsync();

        serviceResponse.Data = _context.Characters.Select(c => _mapper.Map<GetCharacterDto>(c)).ToList();

    }
    catch (Exception ex)
    {

        serviceResponse.Success = false;
        serviceResponse.Message = ex.Message;
    }

    return serviceResponse;
}
```

