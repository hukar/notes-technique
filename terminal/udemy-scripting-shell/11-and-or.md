# 11 `&&`  et `||`

## Le `&&`

`&&`  une deuxième commande est exécuter si et seulement si le code erreur de la première commande est égal à 0.

```bash
ls -l toto.html && mkdir tititi
```

Si `toto.html` n'existe pas, le dossier `tititi` ne sera pas créé (code erreur différent de 0).

Sinon `tititi` sera créé

## Le `||`

`||`  une deuxième commande est exécutée si et seulement si la première a un code erreur différent de 0.

```bash
ls -l toto.html || touch tata.html
```

Si `toto.html` n'existe pas alors crée `tata.html`.

## Composer les deux :

```bash
ping -c 1 8.8.8.98 && echo "joignable" ||
 echo "injoignable"
```

