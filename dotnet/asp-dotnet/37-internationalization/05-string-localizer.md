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
app.MapGet("/translation", (IStringLocalizer<Data> localizer) 
        => localizer[
            "Robot Say Date {0} and Random int {1}", 
            DateTime.Now.ToLongDateString(),
            new Random().Next(0, 21)
        ].Value
    );
```



## `Custom Localizer`

```cs
public class MyCustomLocalizer : IStringLocalizer
{
    public LocalizedString this[string name]
    {
        get { /* */ }
    }
    
    public LocalizedString this[string name, params object[] arguments]
    {
        get { /* */ }
    }
}
```

On l'ajoute à l'application:

```cs
builder.Services.AddSingleton<MyCustomLocalizer>();
```

Et on peut l'injecter où on désire.

> Je n'ai pas réussi à en créer un ... à voire ???
