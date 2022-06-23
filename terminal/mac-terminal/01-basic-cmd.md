# Les commandes de base

## ` whoami` Qui je suis :

````bash
$ whoami
kms
````

## `pwd` où je suis :

Present Working Directory

```bash
$ pwd
/Users/kms
```

## `ls` voire ce qu'il y a ici

**l**i**s**t

```bash
$ ls
Applications	Downloads						Movies			Projects
Desktop				Google Drive				Music				Public
Documents			Library							Pictures

$ ls -a
.										.templateengine
..									Applications
.CFUserTextEncoding	Desktop
.DS_Store						Documents
.IdentityService		Downloads
.ServiceHub					Google Drive
.Trash							Library
.android						Movies
.bash_history				Music
.bash_sessions			Pictures
.config							Projects
.mono								Public

```

`ls -a` montre les fichiers cachés

## Qu'est-ce qu'une commande

Une commande est une ligne qui demande au terminal d'éxécuter une action.

### Il y a trois règles :

1. une commande peut fonctionner seul : `date`
2. une commande peut fonctionner sur uquelque chose : `echo "quelque chose"`
3. Une commande peut avoir des options lui perméttant de varié son fonctionnement : `ls -a`

## Quelques commandes

### `date`

```bash
$ date
Ven 29 mar 2019 14:58:02 CET
$ date -u
Ven 29 mar 2019 13:58:05 UTC
```

`date -u` pour obtenir la date universelle UTC

### `echo`

Pour écrire quelque chose

`echo -n`  pour retirer le saut de ligne à la fin

```bash
$ echo "toto"
toto
$ echo -n "titi"
titi$ _
```

#### écrire sur plusieurs lignes

```bash
$ echo "
> line one
> line two
> line three
> bye bye "

line one
line two
line three
bye bye 
```

On peut écrire sur plusieurs ligne en appuyant sur `enter` et en gardant les guillemets ouverts.

### say

Fait parler le mac :-)

```bash
$ say "je suis ton père"
```



### `killall`

Arrête un processus

```bash
$ killall Notes
```

### `cal`

Affiche le calendrier du mois et de l'année passés en paramètre

```bash
$ cal 10 1975
 Octobre 1975      
Di Lu Ma Me Je Ve Sa  
          1  2  3  4  
 5  6  7  8  9 10 11  
12 13 14 15 16 17 18  
19 20 21 22 23 24 25  
26 27 28 29 30 31
```

Si on utilise juste `cal` on obtient le calendrier du mois en cours 

```bash
$ cal
     Mars 2019        
Di Lu Ma Me Je Ve Sa  
                1  2  
 3  4  5  6  7  8  9  
10 11 12 13 14 15 16  
17 18 19 20 21 22 23  
24 25 26 27 28 29 30  
31
```

