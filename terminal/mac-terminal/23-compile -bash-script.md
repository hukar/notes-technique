# 23 Compiler un script bash

On peut parfois vouloir compiler un script en exécutable binaire.

## Pour cela on utilise :`shc`

https://github.com/neurobin/shc

```
brew install shc
```

## Compiler un script

fichier script : `open_firefox.sh`

```bash
#! /bin/bash
/Applications/Firefox.app/Contents/MacOS/firefox -url about:profiles
```

Ouvre firefox à une page bien précise.

Dans le terminal

```bash
shc -f open_firefox.sh -o opfire
```

<img src="assets/Screenshot2020-09-20at10.51.49.png" alt="Screenshot 2020-09-20 at 10.51.49" style="zoom:50%;" />

On obtient un fichier `c` puis un exécutable.

il suffit de double cliquer sur l'`exec` pour le lancer.
