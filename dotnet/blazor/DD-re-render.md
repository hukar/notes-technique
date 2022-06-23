# DD Re-rendre une page



## `StateHasChanged`

Pour forcer un re-rendu d'une page (un composant page), on utilise la méthode `StateHasChanged` :

```cs
public async void GetColour2()
{
    Colour2 = await httpClient.GetFromJsonAsync<Colour>(urlColour);

    StateHasChanged();
}
```

