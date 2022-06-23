# 08. Créer ses propres `attributes`

## Sommaire

<img src="assets/Screenshot2020-10-23at10.25.17.png" alt="Screenshot 2020-10-23 at 10.25.17" style="zoom:50%;" />

## Où un `custom attribute` peut être appliqué

<img src="assets/Screenshot2020-10-23at11.42.34.png" alt="Screenshot 2020-10-23 at 11.42.34" style="zoom:50%;" />

Voici les éléments sur lesquels un `attribute` peut être appliqué.

### Cible `AttributeTargets.Property`

<img src="assets/Screenshot2020-10-23at11.44.21.png" alt="Screenshot 2020-10-23 at 11.44.21" style="zoom:50%;" />

```csharp
[AttributeUsage(AttributeTargets.Target | AttributeTargets.OtherTarget)]
```

### Nombre d'utilisation `AllowMultiple = true`

Par défaut un `attribute` ne peux être utilisé qu'une fois par fichier de code.

<img src="assets/Screenshot2020-10-23at11.48.04.png" alt="Screenshot 2020-10-23 at 11.48.04" style="zoom:50%;" />

Par défaut on a `[AttributeUsage(Target, AllowMultiple = false)]`.

Les deux premiers exemples sont donc équivalent.

### Héritage `inherited = false`

<img src="assets/Screenshot2020-10-23at11.50.25.png" alt="Screenshot 2020-10-23 at 11.50.25" style="zoom:50%;" />

Les deux premiers exemple sont identiques (valeur par défaut).
