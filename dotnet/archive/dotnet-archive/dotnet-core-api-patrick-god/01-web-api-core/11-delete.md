# La méthode `delete`

## 1. Modifier `ICharacterService`

```cs
Task<ServiceResponse<List<GetCharacterDto>>> DeleteCharacter(int id);
```



## 2. Ajouter la méthode dans `CharacterService`

```cs
public async Task<ServiceResponse<List<GetCharacterDto>>> DeleteCharacter(int id)
{
    ServiceResponse<List<GetCharacterDto>> serviceResponse = new();

    try
    {
        var character = characters.First(c => c.Id == id);

        characters.Remove(character);

        serviceResponse.Data = characters.Select(c => _mapper.Map<GetCharacterDto>(c)).ToList();

    }
    catch (Exception ex)
    {

        serviceResponse.Success = false;
        serviceResponse.Message = ex.Message;
    }

    return serviceResponse;
}
```

On utilise la méthode `First` au lieu de `FirstOrDefault` car elle génère une erreur.

Ensuite c'est la méthode `Remove` qui prend l'élément à retirer de la liste en paramètre.



## 3. Ajouter un méthode au contrôleur

```cs
[HttpDelete("{id}")]
public async Task<ActionResult<ServiceResponse<List<GetCharacterDto>>>> DeleteCharacter(int id)
{
    var response = await _characterService.DeleteCharacter(id);

    if (response.Data is null)
    {
        return NotFound(response);
    }

    return Ok(response);
}
```

#### ! bien ajouter `"{id}"` à l'attribut.

