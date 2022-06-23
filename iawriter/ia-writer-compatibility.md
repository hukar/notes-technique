# `ia writer` compatibilité

Pour utiliser `ia writer` sur `Ipad`, mes fichiers `markdown` ne doivent pas contenir d'espace dans le chemin des images.

## Retirer les espaces dans la balise `<img>`

Avec une regex :

```js
(src="assets\/\S+)\s(\S+)\s(\S+)\s(\S+")
```

`\S` tout caractère non-espace

Avec `VSCode` et l'outil rechercher/remplacer.

<img src="assets/Screenshot2020-12-02at16.33.21.png" alt="Screenshot 2020-12-02 at 16.33.21" style="zoom:50%;" />

```js
$1$2$3$4
```

## Modifier le nom de fichier 

### Pour retirer un espace dans le nom d'un fichier

```bash
for file in *; do mv "$file" "${file// /}"; done
```

### Pour retrouver tous les dossiers `assets`

```bash
find . -name "*.png" | while read f; do mv "$f" "${f// /}"; done
```



## Changer le nom des fichier générer par le screenshot

Pas de solution satisfaisante.

Il faut renommer le `screenshot` avant de le glisser dans Typora.