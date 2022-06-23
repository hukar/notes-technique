# 09 Introduction au scripting

`script.txt`

```bash
# le programme qui interprète et qui éxécute le script

- lignes de script
- lignes de script
- lignes de script
- lignes de script
```

```bash
#! shabang + chemin du rpogramme qui interprète et éxécute le script
```

```bash
#! /bin/bash
```

```bash
which bash
/bin/bash

#1 si nécéssaire on peut utiliser which python; which php; ...
```

## Rendre le script éxécutable

```bash
chmod +x script.txt
```

## extension .sh

Premier script :

```bash
#! /bin/bash

echo "hello I am a script"
read -p "What's your name " username

echo "hello $username nice to meet you"
read -p "tell me the path of the file you want to open " filepath

open $filepath
echo "bye bye ---_<(<)/°= "
```

####! ouvrir un fichiers sous linux `xdg-open monfichier.txt`

