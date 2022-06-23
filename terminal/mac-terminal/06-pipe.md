# 06 `pipe |`

La sortie d'une commande est l'entrée d'une autre commande

```bash
ls -a | less

# est équivalent à
ls -a > temp.txt
kms$ less temp.txt
```

## `tail`

affiche la fin d'un fichier

```bash
tail -4 files.txt
# affiche les 4 dernières lignes de files.txt

Pictures
Projects
Public
files.txt
```

### enchaîner les `pipe`

```bash
ls -a | tail -3 | less
```

### Rediriger la sortie d'un `pipe`vers un fichier

```bash
ls -lahF | tail -5 > five-last.txt
```

