# 04 créer et éditer des fichiers

## `touch`

Commande chageant le timestamp d'un fichier.

Si le fichier n'existe pas, elle en crée un vide.

`-c`  ne pas créer de fichier s'il n'existe pas.

### Créer plusieurs fichiers dans une boucle for

```bash
for i in `seq 1 4`
do touch file${i}root.txt
done
`seq 1 4` # séquence de un à quatre
${i} # reprendre la valeur de i dans la chaîne
```

`for do done`



## `nano`

`ctrl` + `a` et `ctrl` + `e`  pour aller au début ou à la fin de la ligne.

`ctrl` + `v` et `ctrl` + `y` pour  aller en bas ou en haut du texte.

Sinon : `nano -m`  comme **m**ouse pour avoir un curseur dans nano.

## `mv`

### renommer un fichier ou un dossier

```bash
mv titi.txt toto.txt
mv C-Folder CC-Folder 
```



### Déplacer un fichier ou un dossier

```bash
mv titi.txt A-Folder
mv B-Folder A-Folder
```

## `cp`

`-R`  recursive copie les dossiers et leur contenu

`-v`  verbose explicite en texte chaque dossier et fichier copié.

#### ! différence entre copier le dossier ou bien le contenu du dossier :

```bash
# copier le contenue
cp -R A-Folder/ B-Folder/
# copier le dossier
cp -R A-Folder b-Folder/
```

### `A-Folder`  == Le dossier et son contenu

### `A-Folder/`  == Le contenu du dossier



## `rm`

`-R`  pour supprimer les dossiers

`rm *`  supprime tous les fichiers mais pas les répertoires.

#### Supprimer certain fichier

```bash
rm {un-test,nano-m}.txt
```

#### Supprimer un dossier et ses fichiers sans demande d'autorisation pour chaque :

`rm -rf`

```bash
rm -rf *

# si encore des problèmes
sudo rm -rf *
```



## `*`

Avoir des infos sur tous les fichiers de mon répertoire courant :

```bash
file *
# de tous les fichiers texte
file *.txt
# de tous les fichiers commençant par a
file a*
# de tous les fichiers contenant la lettre b
file *b*
```

## `>` redirect

La redirection écrase la même ligne dans le même fichier :

```bash
echo "titi" > newtiti.txt
cat newtiti.txt 
titi
echo "hello new titi" > newtiti.txt 
cat newtiti.txt 
hello new titi
```

La première ligne a été effacée.

## `>>`  append

Ajouter à la fin.

```bash
echo "une nouvelle ligne sur titi" >> newtiti.txt 
cat newtiti.txt 
hello new titi
une nouvelle ligne sur titi
```

une nouvelle ligne est ajoutée à la fin.

## Utilisation des redirections avec d'autres commandes

### `ls`

```bash
ls -lhFa > file-log.txt
cat file-log.txt 
total 24
drwx------+ 14 kms  staff   448B  3 avr 16:11 ./
drwxr-xr-x+ 28 kms  staff   896B  3 avr 14:54 ../
-rw-r--r--@  1 kms  staff   6,0K  3 avr 15:24 .DS_Store
-rw-------   1 kms  staff     0B 19 mar 13:43 .localized
...
```

### `cat`

Qui a bien le sens de concatenate ici

```bash
echo "haha" > a.txt
echo "héhéhéhéhé" > e.txt
cat a.txt e.txt > story.txt
cat story.txt
haha
héhéhéhéhé
```



## `mkdir`

### `mkdir -p`

Pour construire une arborescence :

```bash
mkdir -p programmation/udemy/mac-terminal
```

