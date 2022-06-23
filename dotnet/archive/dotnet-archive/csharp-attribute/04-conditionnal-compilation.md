# 04 Compilation conditionnelle : `[Conditional]`

Permet de compiler des parties de code seulement au `Debug` ou seulement en `Release`.

```csharp
public void WriteAge()
{
    OutputDebugInfo();
    WriteLine(_contact.AgeInYears);
}

[Conditional("DEBUG")]
public void OutputDebugInfo()
{
    WriteLine("***DEBUG MODE***");
}
```

```bash
🦄 csharp-attribute dotnet run -c Release
Sara
42
Hit enter to close program ...

🦄 csharp-attribute dotnet run -c Debug
Sara
***DEBUG MODE***
42
Hit enter to close program ...

```

`-c` configuration : `Debug` par défaut.



## Custom condition

On peut créer ses propres `label` de condition.

Il semble que la règle soit que le label doit être en majuscule et qu'il peut être appelé sans faire attention à la casse.

```csharp
public void WriteAge()
{
    OutputExtraInfo();
    OutputDebugInfo();
    WriteLine(_contact.AgeInYears);
}

// ...
[Conditional("EXTRA")] // ici EXTRA doit obligatoirement être en majuscule
public void OutputExtraInfo()
{
    WriteLine("***EXTRA INFO***");
}
```

```bash
🦄 csharp-attribute dotnet run -c eXtRa # ici c'est insensible à la casse
Sara
***EXTRA INFO***
42
Hit enter to close program ...
```

