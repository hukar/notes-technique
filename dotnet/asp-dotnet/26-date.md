# 26 Les `Date`

## Formater une `Date` avec `ToString`

```cs
DateCreated.Date.ToString("dd MMMM yyyy")
```

`DateCreated` étant de type `DateTime`.



## Trouver le nombre de jours entre deux `Dates`

```cs
(DateIn.Date - DateOut.Date).TotalDays.ToString()
```



### Récupérer la valeur d'une `Date Nullable`

Si on a :

```cs
DateTime? MyDate { get; set; } 
```

Pour récupérer la valeur on doit utiliser `Value` :

```cs
(booking.DateIn.Value - booking.DateOut).TotalDays.ToString()
```

On utilise `Value` car `DateTime` est un `Value Type` (`Struct`).

