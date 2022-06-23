# 01 Udemy git intro

**git** distribué <> **svn** centralisé

**git** peut être utilisé sans connexion au serveur central, il est distribué et décentralisé.

**svn** nécessite obligatoirement une connexion au serveur de gestion de version.

## Configuration de git

```bash
git config --global user.name "Michel Michelot"
git config --global user.email "m.m@titi.com"
```

Visionner la config :

```bash
git config --global --list
```

```bash
user.name=Karim Meshoub
user.email=k.meshoub@gmail.com
user.mail=k.meshoub@gmail.com
color.ui=true
color.diff=auto
color.status=auto
color.branch=auto
filter.lfs.clean=git-lfs clean -- %f
filter.lfs.smudge=git-lfs smudge -- %f
filter.lfs.process=git-lfs filter-process
filter.lfs.required=true
alias.co=checkout
alias.ci=commit
alias.st=status
alias.br=branch
alias.hist=log --pretty=format:'%h %ad | %s%d [%an]' --graph --date=short master --all
alias.type=cat-file -t
alias.dump=cat-file -p
```

