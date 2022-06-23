# 02 Variables Bash

On peut exécuter une commande dans une variable avec la syntaxe *back quote* :

```bash
VARIABLE=`hostname -s` # nom de la machine raccourci
```

Autre exemple :

```bash
LISTE=`ls -l`;
echo "liste : $LISTE";
```

```bash
liste : total 24 # total du poid des fichiers
-rwxr-xr-x  1 hukar  staff  114 13 jan 18:40 bash-test.sh
-rwxr-xr-x  1 hukar  staff   34 13 jan 07:44 php.sh
-rwxr-xr-x  1 hukar  staff   39 13 jan 07:42 python.sh
```

###! le nom des variables ne peut contenir que des lettres majuscule ou minuscule, des chiffres mais pas en premier caractère et des tires bats

```bash
hukar: script-shell $ 4jojo="jojo" # pas de chiffre en premier
bash: 4jojo=jojo: command not found

hukar: script-shell $ jojo4="jojo" # syntaxe correcte

hukar: script-shell $ mi@mi="mimi" # @ caractère non permis
bash: mi@mi=mimi: command not found

hukar: script-shell $ mimi-mimi="mimi" # - caractère non permis
bash: mimi-mimi=mimi: command not found
```

