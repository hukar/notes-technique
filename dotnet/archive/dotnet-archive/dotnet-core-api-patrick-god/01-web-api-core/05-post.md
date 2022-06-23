# 05 Méthode `POST`

## Méthode `PUT`

**Précisions**

La méthode remplace une ressource par une autre entièrement.

Même si seulement une propriété de la ressource a été modifié, c'est toute la ressource qui est substituée par la nouvelle.

#### C'est par essence une mise à jour de l'objet entier



## Méthode `POST`

```cs
[HttpPost]
public ActionResult<List<Character>> AddCharacter(Character newCharacter)
{
    characters.Add(newCharacter);

    return Ok(characters);
}
```

<img src="assets/post-in-swagger-good-work.png" alt="post-in-swagger-good-work" style="zoom:50%;" />

on voit que la `class` est écrite avec un `string` et pas un `int` (grâce à `[JsonConverter]`).

Cette fois le paramètre de la méthode correspond au `body` de la requête et non à l'`url`.