# BB Un exemple

```cs
// See https://aka.ms/new-console-template for more information
Console.WriteLine("Hello, World!");

string Traitment(string nroTask)
{
    for (int i = 0; i < 3; i++)
    {
        Thread.Sleep(2000);
        Console.WriteLine($"Running task {nroTask} loop{i} ... [ProcessId {Thread.CurrentThread.ManagedThreadId}]");
    }
    return $"Ok Async Long Task {nroTask}";
}

void LongProcess()
{
    Console.WriteLine($"Start long Process  [ProcessId {Thread.CurrentThread.ManagedThreadId}]");

    for (int i = 0; i < 3; i++)
    {
        Thread.Sleep(2000);
        Console.WriteLine($"Running loop{i} ... [ProcessId {Thread.CurrentThread.ManagedThreadId}]");
    }

    Console.WriteLine($"result : Ok Long Task");
    Console.WriteLine("Finish long Process\n");
}
async Task LongProcessAsync(string nroTask)
{
    Console.WriteLine($"Start long Process async {nroTask}  [ProcessId {Thread.CurrentThread.ManagedThreadId}]");

    var resultOne = await Task.Run(() => Traitment(nroTask));
    
    Console.WriteLine($"\nBetween two await {nroTask}  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

    var resultTwo = await Task.Run(() => Traitment(nroTask));
    Console.WriteLine($"result one : {resultOne} result two : {resultTwo}");
    Console.WriteLine($"Finish long Process async {nroTask}  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
}

Console.WriteLine($"Before long Process  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
LongProcess();

Console.WriteLine("\nSOMETHING ELSE ONE\n");
Console.WriteLine("Before long Process async\n");
Task taskOne = LongProcessAsync("one");
Task taskTwo = LongProcessAsync("two");
Task taskThree = LongProcessAsync("three");
Console.WriteLine("\nSOMETHING ELSE TWO\n");

Console.WriteLine($"After long Process async  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");

Task.WaitAll(taskOne, taskTwo, taskThree);
Console.WriteLine($"In the end  [ProcessId {Thread.CurrentThread.ManagedThreadId}]\n");
```

