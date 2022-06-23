## Commandes de base

`pwd` : donne le chemin du répertoire où l'on se trouve **print working directory**

```bash
/Users/hukar
```

### Raccourcir le nom dans le terminal

dans `\~\.profile`  ajouter la ligne :

```bash
export PS1="\u$ "
```

`\u` : user name

`\W` : nom du répertoire courant

`\w` : adresse du répertoire courant 

### Aller au dernier endroit visité

`cd -` : ce rend au dernier répertoire