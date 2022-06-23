# 04 `VSCode` tips



## Affichage et taille des polices

Meilleur réglage pour moi :

```json
{
    "window.zoomLevel": 1,
    "editor.fontSize": 18,
    "terminal.integrated.fontSize": 12
}
```

On peut bien sûr définir cela de manière globale dans `User Settings UI`.



## `assets` génération

Pour ajouter les fichiers de configuration `.vscode` :

`cmd`+`shift`+`p` => taper `assets`

<img src="assets/vscode-1.png" style="zoom:50%;" />

## Auto save

<img src="assets/vscode-2.png" style="zoom:50%;" />

## Masquer les dossiers `bin` et `obj`

<img src="assets/vscode-3.png" alt="Screenshot 2020-11-03 at 11.03.53" style="zoom:33%;" />

## Utiliser l'affichage normal des dossiers 

Décocher `Compact Folder` pour ne pas avoir les dossiers vides représentés sur un ligne.

<img src="assets/vscode-4.png" alt="Screenshot 2020-11-03 at 11.09.24" style="zoom:33%;" />



## Voire les `Quick Fix`

### `cmd` + `shift` + `;`



## Auto Completion

### `ctrl` + `space`



## Ajouter un `emoji`

Valable partout (pas seulement `vscode`)

### `ctrl` + `cmd` + `space`

## Voire les constructeur disponible et les générés

<img src="assets/vscode-5.png" alt="Screenshot 2020-11-03 at 16.05.35" style="zoom:33%;" />

`cmd` + `shift` + `;`

## Voire les paramètres possible (`overload`) d'une méthode

### `cmd` + `shift` + `space`

<img src="assets/vscode-6.png" alt="Screenshot 2020-11-03 at 16.16.19" style="zoom:33%;" />

## Configurer les membres privés `C# extension`

<img src="assets/vscode-7.png" alt="Screenshot 2020-11-03 at 16.52.37" style="zoom:33%;" />

<img src="assets/vscode-8.png" alt="Screenshot 2020-11-03 at 16.53.08" style="zoom:33%;" />

Résultat du code généré

```csharp
public Startup(IConfiguration config)
{
}
```

<img src="assets/vscode-9.png" alt="Screenshot 2020-11-03 at 16.55.58" style="zoom:33%;" />

```csharp
private readonly IConfiguration _config;

public Startup(IConfiguration config)
{
    _config = config;
}
```



## Utiliser `Emmet` avec `Razor`

Il faut ajouter dans `.vscode/settings.json` :

```json
"emmet.includeLanguages": {
    "aspnetcorerazor": "html"
}
```

ensuite tout fonctionne correctement.

<img src="assets/emmet-with-razor.png" alt="emmet-with-razor" style="zoom:67%;" />

