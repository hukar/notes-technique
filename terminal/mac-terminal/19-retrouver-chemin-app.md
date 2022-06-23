# 19 Retrouver l'emplacement d'une application ou d'une commande

## `type`  `which`  `locate`

```bash
kms: ~ $ type find
find is /usr/bin/find
```

```bash
kms: ~ $ type -p find
/usr/bin/find
```

```bash
kms: ~ $ which find
/usr/bin/find
```

```bash
kms: ~ $ locate find | fgrep -w bin
/Applications/MAMP/bin/phpMyAdmin/js/tbl_find_replace.js
/Applications/MAMP/bin/phpMyAdmin/tbl_find_replace.php
/Applications/MAMP/bin/phpMyAdmin/themes/original/img/b_find_replace.png
/Applications/MAMP/bin/phpMyAdmin/themes/pmahomme/img/b_find_replace.png
/Library/Frameworks/Mono.framework/Versions/5.16.1/bin/mono-find-provides
/Library/Frameworks/Mono.framework/Versions/5.16.1/bin/mono-find-requires
/Library/Frameworks/Mono.framework/Versions/6.0.0/bin/mono-find-provides
/Library/Frameworks/Mono.framework/Versions/6.0.0/bin/mono-find-requires
/System/Library/Frameworks/Python.framework/Versions/2.7/Extras/bin/macho_find
```

`which` et `type` s'utilise si le programme set dans le `path`

`locate`cherche partout, à coupler avec un `fgrep`.

`fgrep` est identique à `grep -F` : --fixed-string : le motif est une chaîne de caractère