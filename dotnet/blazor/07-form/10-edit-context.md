# 10 `EditContext`

Pour travailler avec `EditContext` dans la partie `@code`, on crée une féférence sur un `EditForm` et on obtient une référence vers `EditContext`:

```cs
<EditForm @ref="myForm" Model="Person">
  
  // ...
@code {
  EditForm? myForm;
  
  void MySuperMethode()
  {
    // pour lancer la validation
    myForm.EditContext.Validate();
    
    // Pour savoir si le formulaire a été "touché"
    if(myForm.EditContext.IsModified())
    {
      // ... 
```

### `context.Validate()`

Renvoie un booléen `true`si valide et déclenche la validation du formulaire.



### `context.IsModified()`

Permet de savoir si le formulaire a déjà été modifié (une valeur est entrée) ou s'il est "vierge".

Ajouter et retirer le `focus` ne compte pas comme une modification.

Par contre écrire quelque chose et l'effacer fait passer `IsModified`  à `true`.