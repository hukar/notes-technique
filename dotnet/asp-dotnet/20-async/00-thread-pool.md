# 00 Le `Thread Pool`

## Connaitre le nombre de `Thread`

### Manière empirique

```cs
using System.Diagnostics;

Stopwatch timer = new();

timer.Start();

var loadingsTask = new List<Task>();

int nbTasks = 40;

for(int i = 0 ; i < nbTasks; i++)
{
    var loadTask = GetOk();

    loadingsTask.Add(loadTask);
}

await Task.WhenAll(loadingsTask);
Console.WriteLine($"nombre de Task exécutées : {nbTasks}");

timer.Stop();

Console.WriteLine(timer.Elapsed.Seconds + " secondes");
Console.WriteLine($"nombre de Thread : {nbTasks/timer.Elapsed.Seconds}");
```
```cs
Task GetOk() => Task.Run(() => { Thread.Sleep(1000); });
```

```bash
nombre de Task exécutées : 40
3 secondes
nombre de Thread : 13
```



## Connaitre le nombre de `Thread`

### `ThreadPool.GetMinThreads`

```cs
ThreadPool.GetMinThreads (out int workerThreadsMin, out int completionPortThreadsMin);
Console.WriteLine($"GetMinThreads : {workerThreadsMin} {completionPortThreadsMin}");
```

Donne le nombre de `Thread` minimum (déterminé sur les caractéristique du `CPU`).

```bash
GetMinThreads : 12 12
```

Le nombre idéal de `Thread` est le nombre de `CPU` (ma machine a 6 core physique => 12 cores logiques).

### `ThreadPool.GetMaxThreads`

```cs
ThreadPool.GetMaxThreads(out int workerThreads, out int completionPortThreads);
Console.WriteLine($"ThreadPool.GetMaxThreads : {workerThreads} {completionPortThreads}");
```

```cs
ThreadPool.GetMaxThreads : 32767 1000
```

Ce sont des maximum théorique.



##  Modifier le nombre de `Thread` en Parallèle

### `SetMinThreads` et `SetMaxThreads`

En jouant sur ces deux valeurs, on peut modifier les temps d'exécution :

> Pour aller en dessous du minimum il faut régler à la fois le `minimum` et le `maximum` sur `4`.

```cs
ThreadPool.SetMinThreads(4, 12);
ThreadPool.SetMaxThreads(4, 1000);
```

Avec le code précédent on obtient :

```
nombre de Task exécutées : 40
10 secondes
nombre de Thread : 4
```

Ou encore

> Ici on souhaite dépasser le `minimum`, le `maximum` est déjà assez élevé.

```cs
ThreadPool.SetMinThreads(40, 12);
```

```
nombre de Task exécutées : 40
1 secondes
nombre de Thread : 40
```

Comme chaque `Task` dure `1s`, `40 Tasks` sont exécutées en `1s`.



## Connaitre l'`Id` d'une `Thread`

```cs
using System.Threading;

void ThreadInfoId(string site)
{
    var threadId = Thread.CurrentThread.ManagedThreadId;
    display($"Current : {site} Thread Id : {threadId}");
}

ThreadInfoId("Main Code");

var t = Task.Run(() => ThreadInfoId("In task run"));
await t;
```

```
Current : Main Code Thread Id : 6
Current : In task run Thread Id : 22
```

Si on utilise la méthode `Wait` on remarque qu'on e-reste dans la même `Thread` :

```cs
ThreadInfoId("Main Code");

var t = Task.Run(() => ThreadInfoId("In task run"));
t.Wait();
```

```
Current : Main Code Thread Id : 6
Current : In task run Thread Id : 6
```

