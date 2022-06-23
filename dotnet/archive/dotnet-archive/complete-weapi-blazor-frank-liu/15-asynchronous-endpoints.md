# 15 `endpoints` asynchrones

Dans `MVC Framework` il y a un `threadpool` 

Un `thread` = une pile d'exécution.

Ce n'est pas l'idéal car limité au nombre de `thread` prévu dans le `threadpool`.

L'architecture asynchrone permet d'être plus disponnible pour les requêtes arrivantes, sans forcement attendre qu'une `thread` se libère : modèle non bloquant.

## Implémentation

Il faut ajouter le mot clé `async` sur les `actions` et encadrer les `IActionResult` dans une `Task`.

Il faut aussi utiliser les méthodes `async` de `Linq` contenues dans `entity framework`.

```cs
[HttpGet]
public async Task<IActionResult> Get()
{
    return Ok(await _db.Projects.ToListAsync());
}
```

```cs
[HttpGet("{id}")]
public async Task<IActionResult> GetById(int id)
{
    var project = await _db.Projects.FindAsync(id);

    if (project is null)
    {
        return NotFound();
    }

    return Ok(project);
}
```

```cs
[HttpGet]
[Route("/api/projects/{pid}/tickets")]
public async Task<IActionResult> GetProjectTickets(int pid)
{
    var tickets = await _db.Tickets.Where(t => t.ProjectId == pid).ToListAsync();
    if (tickets is null || tickets.Count <= 0)
    {
        return NotFound();
    }

    return Ok(tickets);
}
```

```cs
[HttpPost]
public async Task<IActionResult> Post([FromBody] Project project)
{
    _db.Projects.Add(project);
    await _db.SaveChangesAsync();

    return CreatedAtAction(nameof(GetById), new { id = project.ProjectId }, project);
}
```

```cs
[HttpPut("{id}")]
public async Task<IActionResult> Put(int id, Project project)
{
    if (id != project.ProjectId)
    {
        return BadRequest();
    }

    _db.Entry(project).State = EntityState.Modified;

    try
    {
        await _db.SaveChangesAsync();
    }
    catch
    {
        if (await _db.Projects.FindAsync(id) is null)
        {
            return NotFound();
        }
        throw;
    }

    return NoContent();
}
```

```cs
[HttpDelete("{id}")]
public async Task<IActionResult> Delete(int id)
{
    var project = await _db.Projects.FindAsync(id);

    if (project is null)
    {
        return NotFound();
    }

    _db.Projects.Remove(project);
    await _db.SaveChangesAsync();

    return Ok(project);
}
```

Idem pour `TicketController`.

