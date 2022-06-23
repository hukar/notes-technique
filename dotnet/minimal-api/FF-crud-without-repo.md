# FF  Création d'un `CRUD` sans `Repository`



## Code : `Program.cs`

### Le `Domain` et le `DbContext` 

à placer en fin de fichier (ici en haut pour la lecture)

```cs
using Microsoft.EntityFrameworkCore;
using static Microsoft.AspNetCore.Http.Results;
```
le `using static` permet une écriture plus lisible sans devoir mettre partout `Results.Something()`.

```cs
var builder = WebApplication.CreateBuilder(args);
builder.Services.AddDbContext<JobSiteDb>(options => {
    options.UseSqlite(
        builder.Configuration.GetConnectionString("SqliteConnection")
    );
});

var app = builder.Build();
```
On ajoute le `DbContext` au conteneur de `Services`.



### `Read All`

```cs
app.MapGet("/", async (JobSiteDb db) => await db.Jobs.ToListAsync());
```



### `Read One`

```cs
app.MapGet("/{Id:int}", async (JobSiteDb db, int Id) 
    => await db.Jobs.FindAsync(Id) is Job job ? Ok(job) : NotFound());
```



### `Create`

```cs
app.MapPost("/", async (JobSiteDb db, Job job) => {
    db.Add(job);
    await db.SaveChangesAsync();
    return Created($"/{job.Id}", job);
});
```

alternative en utilisant `HttpResponse` :

```cs
app.MapPost("/", async (JobSiteDb db, Job job, HttpResponse response) => {
    db.Jobs.Add(job);
    await db.SaveChangesAsync();
    
    response.StatusCode = 201;
    response.Headers.Location = $"/{job.Id}";
});
```



### `Update`

```cs
app.MapPut("/{Id:int}", async (JobSiteDb db, int Id, Job newJob) => {
    var job = await db.Jobs.FindAsync(Id);

    if(job is null)
    {
        return BadRequest();
    }

    job.Name = newJob.Name;
    await db.SaveChangesAsync();
    return NoContent();
});
```



### `Delete`

```cs
app.MapDelete("/{Id:int}", async (JobSiteDb db, int Id) => {
    var jobToDelete = await db.Jobs.FindAsync(Id);
    if(jobToDelete is null)
    {
        return BadRequest();
    }
    db.Remove(jobToDelete);
    await db.SaveChangesAsync();
    return NoContent();
});
```

Lancer l'application
```cs
app.Run();
```





