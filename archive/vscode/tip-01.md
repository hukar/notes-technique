#Astuces visual studio code
### pop-up de proposition

```json
 "editor.quickSuggestions": {
        "other": true,
        "comments": false,  // dans les commentaires
        "strings": false  // dans les chaînes de caractères
    }
```

### Automatiser la compilation

`f5` génère un fichier `launch.json` 

###Rechercher un fichier

menu command-palette `ctrl+shift+p` enlever `>`  et taper le nom de fichier

###Faire fonctionner les snippets

dans les préférences :

```json
"editor.snippetSuggestions": "top"
```

ex : **log** + `enter` -> `console.log('$0')`

### Permettre les commentaires dans les fichiers `.json`

dans `.vscode/settings.json` :

```json
{
    "files.associations": {
        "*.json": "jsonc"
    },
    
    // ...
}
```

