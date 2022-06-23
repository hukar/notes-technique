# ouvrir un fichier dans l'application

```sh
open rep/nom_de_fichier.md
```

Va ouvrir automatiquement dans l'application par default

```sh
open -a /Applications/Atom.app mon_fichier.js
```

-a Permet de spécifié l'application pour ouvrir

# recherche d'un répertoire

```sh
find ~ -type d -name "notes-techniques"
```

retourne le chemin du répertoire (-type d ou f pour directory et file) 

# copier un fichier

```sh
cp unfichier.jpg macopie.jpg
```

-R recursivité dans un répertoire

```sh
cp -R monrepetoir autrerepertoire
```

Tous les fichiers jpg dans le répertoire images

```sh
cp *.jpg ~/images
```

# renommer un fichier

```sh
mv ancienNom.md nouveaunom.md
```

Déplacer des fichiers dans un répertoire

```sh
mv *.md ~/doc/doc-technique/
```

# créer plusieurs fichiers avec la boucle for

```sh
for i in "patate" "saucisse" "camenbert"
do
touch monrep/"$i"
done
```


# retrouver quelque chose

```sh
whereis gcc

## ou bien

find / -iname gcc
```

