# 00 Astuces

## `cmd + k`  super clear

Pour vraiment effacer le terminal : `cmd` + `k`

## `history`

Affiche les 500 dernières commandes.

Les cinq dernières :  `history 5`

## `man`

### Recherche dans `man`

`/` + `text` + `enter`  va surligner le texte rechercher dans le document de `man`

`n`  prochaine occurence (**n**ext)

`shift` + `n` retour vers la précédente occurence



## `tab`

S'il y a plusieurs possibilités on peut appuyer deux fois su `tab`.

## `ctrl` + l == `clear`

## `alt`  + mouse

Aller sur un endroit précis de la commande avec la souris.

## `ctrl` + `a` et `ctrl` + `e`

Aller au début ou à la fin de la ligne de commande.

## `ctrl` + `k` et `ctrl` + `y`

Il faut être au début de la ligne de commande ( `ctrl` + `a` )

Couper / coller une ligne de commande.

## Changer son prompt

Par default `PS1` vaut :

```bash
echo $PS1
\h:\W \u\$
```

Ce qui donne Home directory : Working directory User $

```bash
MBP-de-karim:.bash_sessions kms$
```

Pour plus de détails :

> ```
> \d – Date courante
> \t – Heure courante
> \h – Nom de l'hôte
> \# – Numéro de la commande
> \u – Nom d'utilisateur
> \W – Répertoire de travail courant (exemple: Desktop)
> \w – Répertoire de travail courant, chemin complet (exemple: /Users/Admin/Desktop)
> ```

Maintenant on crée un fichier `.bash_profile`

```bash
nano .bash_profile
```

```bash
# .bash_profile
PS1="\u : \W $ "
```

Puis on rafraichi :

```bash
source .bash_profile
```

