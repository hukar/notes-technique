# 05 Utiliser les `String Localizer`



## Les interfaces de `String Localizer`

Il y a deux `interfaces` utilisables : `IStringLocalizer/IStringLocalizer<T>` et `IHtmlLocalizer/IHtmlLocalizer<T>`



### `IStringLocalizer`/`IStringLOcalizer<T>`

Localise un `string`.

Possède les méthodes `.GetString()` et `.GetAllStrings()`.



### `IHtmlLocalizer`/`IHtmlLocalizer<T>`

Localise un `string` avec `html`, la sortie est considérée comme `html`.

On a la méthode `.GetHtml()` qui retourne un `Localized HTML String`.

Il y a aussi `.GetString()` et `.GetAllStrings()`.



## `Localizer` un message complexe

On va utiliser un `string` paramétré à tous les niveaux.



### Dans les fichiers `.resx`

<img src="assets/string-interpolation-resx-files.png" alt="string-interpolation-resx-files" />



### Utilisation avec le service `Localizer` dans un `endpoint`

```cs
```

