# 26 changer le nom de plusieurs fichiers

## Modifier le nom de fichier 

### Pour retirer un espace dans le nom d'un fichier

```bash
for file in *; do mv "$file" "${file// /}"; done
```

### Pour retrouver tous les fichier d'un type

```bash
find . -name "*.png" | while read f; do mv "$f" "${f// /}"; done
```

