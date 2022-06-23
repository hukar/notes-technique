# se ballader dans les commit

lister les identifiants en raccourci :

```sh
git log --oneline

883bf83 (HEAD) affichage du rapport solo bug de la discussion
a7afda5 fin du codage module 1.2
331e1f7 correction de dashboard + finalisation de MODAL
d6ea527 reprise des opérations
dd467d0 boite modal de confimation
555ca7a travail sur report show
```

charger un commit

```sh
git reset --hard d6ea527
```

`--hard` modifie le répertoire de travail

revenir au dernier commit

```sh
git reset --hard 883bf83
```

annuler les dernières modification non commitées

```sh
git reset --hard
```
équivaut à `git reset --hard HEAD`
