# 12 Inverser un `string`

## Méthode avec un `char[]`

```cs
var str = "Hello, World!";

char[] charArray = str.ToCharArray();

Array.Reverse(charArray); // Reverse viebnt ici du namespace System

Console.WriteLine(new String(charArray));
```

```
!dlroW ,olleH
```



## Méthode rapide avec `Reverse()` de `Linq`

```cs
Console.WriteLine( new String( "Yop Yop".Reverse().ToArray() ) );
```

```
poY poY
```

