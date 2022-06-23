# AA Chronométre du code

## `Stopwatch`

```cs
using System.Diagnostics;

Stopwatch timer = new();

timer.Start()
  
  DoSomething();

timer.Stop().

Console.WriteLine(timer.ElapsedMilliseconds);
Console.WriteLine(timer.ElapsedTicks); // ultra précis
Console.WriteLine($"{timer.Elapsed.TotalMilliseconds}ms | {timer.Elapsed.Seconds}s : {timer.Elapsed.Milliseconds}ms");  
Console.WriteLine(timer.Elapsed.TotalSeconds);  // plus précis que le premier  
```



## Chronométrer des `Task` en parallèle

```cs
Task<int> GetOk()
{
    return Task.Run(() => {
        Thread.Sleep(1000);
        return 1;
    });
}
```



```cs
Stopwatch timer = new();

timer.Start();

var loadingsTask = new List<Task<int>>(); 

for(int i = 0 ; i < 40; i++)
{
    var loadTask = GetOk();
    loadingsTask.Add(loadTask);
}

var result = await Task.WhenAll(loadingsTask);
Console.WriteLine($"nombre de Task exécutées : {result.Count()}");

timer.Stop();

Console.WriteLine(timer.Elapsed.Seconds + " secondes");
```

```bash
nombre de Task exécutées : 40
4 secondes
```

Pour `i < 80` :

```bash
nombre de Task exécutées : 80
 
6 secondes
```

Ce qui nous donne entre `10` et `13` `Thread`  en parallèle.



## Connaître la précision de `Stopwatch`

```cs
// Display the timer frequency and resolution.
if (Stopwatch.IsHighResolution)
{
    Console.WriteLine("Operations timed using the system's high-resolution performance counter.");
}
else
{
    Console.WriteLine("Operations timed using the DateTime class.");
}
long frequency = Stopwatch.Frequency;
Console.WriteLine($"Timer frequency in ticks per second = {frequency}");
long nanosecPerTick = (1000L * 1000L * 1000L) / frequency;
Console.WriteLine($"Timer is accurate within {nanosecPerTick} nanoseconds\n");
```

```
Operations timed using the system's high-resolution performance counter.
Timer frequency in ticks per second = 1000000000
Timer is accurate within 1 nanoseconds
```

