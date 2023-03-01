# 01 Les `scheduler` dans `.net`

Nick Chapsas https://www.youtube.com/watch?v=J4JL4zR_l-0&ab_channel=NickChapsas



## Dans le framework

Il y a cinq différents `scheduler` (`Timer`)

```
System.Threading.Timer
System.Timers.Timer  
System.Windows.Forms.Timer
System.Web.UI.Timer
System.Windows.Threading.DispatcherTimer
```

On peut encore utiliser dans `.net 6`

```
System.Threading.Timer
System.Timers.Timer 
```

De l'aveu même de `Microsoft`, aucun n'est à utilisé, l'architecture étant trop vieille.

Un nouveau `Timer` a été introduis dans `.net 6` :

```cs
new PeriodicTimer();
```



## Réalisation "manuelle" d'un `Timer`

On va créer une classe `RepeatingService`

```cs
public class RepeatingService : BackgroundService
{
    private readonly ILogger<RepeatingService> _logger;

    public RepeatingService(ILogger<RepeatingService> logger)
    {
        _logger = logger;
    }
    
    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        while(stoppingToken.IsCancellationRequested == false)
        {
            Console.WriteLine(DateTime.Now.ToString("O"));
            await Task.Delay(1000, stoppingToken);
        }
    }
}
```

`"O"` est un format `ISO` (standard)

On enregistre maintenant le `service`:

```cs
builder.Services.AddHostedService<ReapeatingService>();
```

Si on regarde un échantillon de valeur dans la `console`, on se rend compte qu'un décalage continu d'avancer. Chaque laps de temsp est calculé sur le précédent et non pas sur l'heure de départ :

```
2023-02-17T15:23:47.7851320+01:00
2023-02-17T15:23:48.7863920+01:00
...
2023-02-17T15:24:09.7984720+01:00
2023-02-17T15:24:10.7988190+01:00
2023-02-17T15:24:11.7995470+01:00
2023-02-17T15:24:12.7998650+01:00
2023-02-17T15:24:13.8006620+01:00
2023-02-17T15:24:14.8011170+01:00
2023-02-17T15:24:15.8018730+01:00
2023-02-17T15:24:16.8021640+01:00
...
2023-02-17T15:24:42.8198690+01:00
2023-02-17T15:24:43.8207050+01:00
2023-02-17T15:24:44.8218190+01:00
2023-02-17T15:24:45.8224650+01:00
```

ET si maintenant on a une tâche `async` prenant un certain temps :

```cs
protected override async Task ExecuteAsync(CancellationToken stoppingToken)
{
    while(stoppingToken.IsCancellationRequested == false)
    {
        await DoSomethingAsync();
        await Task.Delay(1000, stoppingToken);
    }
}

private async Task DoSomethingAsync()
{
    Console.WriteLine(DateTime.Now.ToString("O"));
    await Task.Delay(500);
} 
```

On voit que les temps s'additionne (!!!) ce qui n'est pas du tout ce qu'on veut d'un `Timer` :

```
2023-02-17T15:32:00.3782620+01:00
2023-02-17T15:32:01.8797600+01:00
2023-02-17T15:32:03.3818380+01:00
2023-02-17T15:32:04.8830490+01:00
2023-02-17T15:32:06.3853190+01:00
2023-02-17T15:32:07.8870350+01:00
```

On a maintenant un peu plus d'une seconde et demi entre chaque tâche.



## Utilisation de `PeriodicTimer`

```cs
public class RepeatingService : BackgroundService
{
    private readonly PeriodicTimer _timer = new(TimeSpan.FromMilliseconds(1000));

    protected override async Task ExecuteAsync(CancellationToken stoppingToken)
    {
        while(await _timer.WaitForNextTickAsync(stoppingToken) && stoppingToken.IsCancellationRequested == false)
        {
            await DoSomethingAsync();
        }
    }

    private async Task DoSomethingAsync()
    {
        Console.WriteLine(DateTime.Now.ToString("O"));
        // await Task.Delay(500);
    } 
}
```

La variation n'avance plus, le délai est calculé par rapport à l'heure de départ :

```
2023-02-17T15:36:24.6058940+01:00
2023-02-17T15:36:25.6051990+01:00
...
2023-02-17T15:36:53.6057230+01:00
2023-02-17T15:36:54.6054760+01:00
2023-02-17T15:36:55.6048130+01:00
...
2023-02-17T15:37:28.6065430+01:00
2023-02-17T15:37:29.6063650+01:00
2023-02-17T15:37:30.6055700+01:00
2023-02-17T15:37:31.6067270+01:00
2023-02-17T15:37:32.6062150+01:00
```

`PeriodicTimer` essaye de rester consistant même si la tâche `schedulée` a elle même un certain temps :

```cs
private async Task DoSomethingAsync()
{
    Console.WriteLine(DateTime.Now.ToString("O"));
    await Task.Delay(500);
} 
```

```
...
2023-02-17T15:41:35.0802980+01:00
2023-02-17T15:41:36.0813680+01:00
2023-02-17T15:41:37.0808240+01:00
2023-02-17T15:41:38.0802550+01:00
2023-02-17T15:41:39.0806470+01:00
2023-02-17T15:41:40.0809360+01:00
2023-02-17T15:41:41.0814790+01:00
```

On garde bien une seconde `1 s` entre chaque exécution et non pas `1,5 s` comme précédemment.

Si le temps est plus grand que le délai, le délai ne s'ajoute pas :

```cs
// toujours avec PeriodicTimer _timer = new(TimeSpan.FromMilliseconds(1000));

private async Task DoSomethingAsync()
{
    Console.WriteLine(DateTime.Now.ToString("O"));
    await Task.Delay(1500);
} 
```

```
...
2023-02-17T15:43:51.8469230+01:00
2023-02-17T15:43:53.3464880+01:00
2023-02-17T15:43:54.8472230+01:00
2023-02-17T15:43:56.3477900+01:00
2023-02-17T15:43:57.8486820+01:00
2023-02-17T15:43:59.3500080+01:00
```

On observe que l'on a `1,5 s` entre chaque exécution, ce qui est logique.



## `Scheduler` dans une application `Console`

Création d'une classe `BackgroundTask.cs`

```cs
public class BackgroundTask
{
   private readonly PeriodicTimer _timer;
   private readonly CancellationTokenSource _cts = new();
   private Task? _timerTask; 

   public BackgroundTask(TimeSpan interval)
   {
        _timer = new(interval);
   }

   public void Start()
   {
        _timerTask = DoWorkAsync();
   }

   private async Task DoWorkAsync()
   {
        try
        {
            while(await _timer.WaitForNextTickAsync(_cts.Token))
            {
                Console.WriteLine(DateTime.Now.ToString("O"));
            }
        }
        catch (OperationCanceledException) { }
   }

   public async Task StopAsync()
   {
        if(_timerTask is null)  return;

        _cts.Cancel();
        await _timerTask;
        _cts.Dispose();

        Console.WriteLine("Task was cancelled");
   }
}
```

Utilisation dans `Program.cs`

```cs
using ConsoleSchedulerTest;

Console.WriteLine("Press any key to Start the Task");
Console.ReadKey();

var task = new BackgroundTask(TimeSpan.FromMilliseconds(1000));
task.Start();

Console.WriteLine("Press any key to Stop the Task");
Console.ReadKey();

await task.StopAsync();
```

Peut être utiliser comme squelette pour une tâche programmée à interval régulier.