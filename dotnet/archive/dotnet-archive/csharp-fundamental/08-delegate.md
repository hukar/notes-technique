# 08 `delegate`

C'est un type.

Seul le type de retour et le type des paramètres doivent correspondre.

En dehors de la classe de test :

```cs
public delegate string WriteLogDelegate(string logMessage);
```

Dans la classe de test :

```cs
[Fact]
public void WriteDelegateCanPointToMethod()
{
    WriteLogDelegate log;

    log = new WriteLogDelegate(ReturnMessage);

    var result = log("hello");
    Assert.Equal("hello", result);
}

string ReturnMessage(string message)
{
    return message;
}
```

Le délégué ressemble à un pointeur vers fonction.

On peut aussi utiliser l'écriture raccourcie :

```cs
WriteLogDelegate log;

log = ReturnMessage;
```

## Exemple d'implémentation

```cs
delegate int ComputeDelegate(int a, int b);

ComputeDelegate FnFactory(int coef)
{
    return (int a, int b) => {
        return coef*a + b;
    };
}

ComputeDelegate MyFuncCoef2;

MyFuncCoef2 = FnFactory(2);

MyFuncCoef2(4, 7);
```

```
15
```

Les `delegate ` permettent bien de créer des types pointeur de fonction.

## Multi-cast Delegates

On peut invoquer plusieurs méthodes avec un `delegate`.

```cs
public delegate string WriteLogDelegate(string logMessage);
    
    
    public class TypeTests
    {
        int count = 0;
        
        [Fact]
        public void WriteDelegateCanPointToMethod()
        {
            WriteLogDelegate log = ReturnMessage;

            log += ReturnMessage;
            log += IncrementCount;

            var result = log("hello");
            Assert.Equal(3, count);
            Assert.Equal("HELLO", result);
        }

        string ReturnMessage(string message)
        {
            count++;
            return message;
        }
        string IncrementCount(string message)
        {
            count++;
            return message.ToUpper();
        }
```

Mon `delegate` peut recevoir plusieurs pointeurs sur fonction :

```cs
WriteLogDelegate log = ReturnMessage;

log += ReturnMessage;
log += IncrementCount;
```

Il va appeler dans l'ordre chacune des fonctions pointées.

Le résultat est donc bien `HELLO` et `count` vaut bien `3`.

### exemple

```cs
delegate int ComputeDelegate(int a, int b);

int Add(int a, int b) 
{
    Console.WriteLine($"{a + b}");
    return a + b;
}

int Substract(int a, int b)
{
    Console.WriteLine($"{a - b}");
    return a-b;
}

int Multiply(int a, int b)
{
    Console.WriteLine($"{a * b}");
    return a * b;
}


ComputeDelegate MyCalculator = Add;
MyCalculator += Substract;
MyCalculator += Multiply;

var result = MyCalculator(9, 2);
Console.WriteLine(result);
```

```
11
7
18
18
```

