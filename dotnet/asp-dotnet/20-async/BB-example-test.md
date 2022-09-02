# BB Un exemple

## Fonctions utilitaires

```cs
void Traitment(bool isAsync)
{
    for (int i = 0; i < 3; i++)
    {
        Thread.Sleep(2000);
        
        Console.WriteLine($"Running task {(isAsync ? "async" : "")} loop{i} ... [ProcessId {Thread.CurrentThread.ManagedThreadId}]");   
    }
    
    Console.WriteLine($"Finish traitment {(isAsync ? "async" : "")}");
}

void LongProcess()
{
    Traitment(false);

    Console.WriteLine("Finish long Process\n");
}

async Task LongProcessAsync()
{
    Console.WriteLine($"Start long Process async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]");

    await Task.Run(() => Traitment(true));
    
    Console.WriteLine($"\nBetween two await [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

    await Task.Run(() => Traitment(true));

    Console.WriteLine($"Finish long Process async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
}
```

## Programme

```cs
// See https://aka.ms/new-console-template for more information
Console.WriteLine("Hello, World Async!");

void Traitment(bool isAsync)
{
    for (int i = 0; i < 3; i++)
    {
        Thread.Sleep(2000);
        
        Console.WriteLine($"Running task {(isAsync ? "async" : "")} loop{i} ... [ProcessId {Thread.CurrentThread.ManagedThreadId}]");   
    }
    Console.WriteLine($"Finish traitment {(isAsync ? "async" : "")}");
}

void LongProcess()
{
    Traitment(false);

    Console.WriteLine("Finish long Process\n");
}

async Task LongProcessAsync()
{
    Console.WriteLine($"Start long Process async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]");

    await Task.Run(() => Traitment(true));
    
    Console.WriteLine($"\nBetween two await [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

    await Task.Run(() => Traitment(true));

    Console.WriteLine($"Finish long Process async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
}

Console.WriteLine($"Before long Process  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

LongProcess();

Console.WriteLine("\nSOMETHING ELSE ONE\n");

Console.WriteLine($"Before long Process async [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");


Task taskOne = LongProcessAsync();

Console.WriteLine("\nSOMETHING ELSE TWO\n");

Console.WriteLine($"After long Processes async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

// Task.WaitAll(taskOne, taskTwo, taskThree);
// await Task.WhenAll(taskOne, taskTwo, taskThree);
// taskOne.Wait();

await taskOne;
Console.WriteLine($"Task is resolved  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```



### décomposition avec l'affichage

```cs
Console.WriteLine($"Before long Process  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```
``` 
Before long Process  [ProcessId 1`
```
```cs
LongProcess();
```
```
Running task  loop0 ... [ProcessId 1]
Running task  loop1 ... [ProcessId 1]
Running task  loop2 ... [ProcessId 1]
Finish traitment 
Finish long Process
```

```cs
Console.WriteLine("\nSOMETHING ELSE ONE\n");
```
```
SOMETHING ELSE ONE
```
```cs
Console.WriteLine($"Before long Process async [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```
```
Before long Process async [ProcessId 1]
```
```cs
Task taskOne = LongProcessAsync();
```
```
Start long Process async  [ProcessId 1]
```
```cs
Console.WriteLine("\nSOMETHING ELSE TWO\n");

Console.WriteLine($"After long Processes async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```
```
After long Processes async  [ProcessId 1]
```
```cs
// taskOne.Wait();

await taskOne;
```
```
Running task async loop0 ... [ProcessId 4]
Running task async loop1 ... [ProcessId 4]
Running task async loop2 ... [ProcessId 4]
Finish traitment async

Between two await [ProcessId 4]

Running task async loop0 ... [ProcessId 6]
Running task async loop1 ... [ProcessId 6]
Running task async loop2 ... [ProcessId 6]
Finish traitment async
Finish long Process async  [ProcessId 6]
```
```cs
Console.WriteLine($"Task is resolved  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```
```
Task is resolved  [ProcessId 6]`
```

L'utilisation de `await` semble transporter le scope d'une `thread` à l'autre.

Si on utilise une méthode bloquante on retombe alors plutôt sur la `thread` de départ :

```cs
taskOne.Wait();

// await taskOne;
Console.WriteLine($"Task is resolved  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```

```
Task is resolved  [ProcessId 1]
```



