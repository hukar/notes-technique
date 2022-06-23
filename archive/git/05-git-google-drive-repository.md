# 05 git google-drive repository

On peut utiliser un drive comme repository :

On doit créer un repository vide dans notre drive :

`Users/hukar/Drive`

```bash
mkdir mon-projet
cd mon-projet
git init --bare workspace
```

`--bare` définit un repository sans espace de travail, typiquement utilisé pour un serveur git.

> Si on crée un repository seulement en faisant `git init` , on ne pourra pas faire de `push` car `git` lance une erreur :

>```bash
>git push file:///Users/hukar/Documents/programmation/udemy_cours_git/clone_mon_site master
>```

>```bash
>remote: error: refusing to update checked out branch: refs/heads/master
>By default, updating the current branch in a non-bare repository
>remote: is denied, because it will make the index and work tree inconsistent with what you pushed, and will require 'git reset --hard' to match
>the work tree to HEAD.
>You can set the 'receive.denyCurrentBranch' configuration variable to 'ignore' or 'warn' in the remote repository to allow pushing into its current branch; however, this is not recommended unless you arranged to update its work tree to match what you pushed in some other way.
>To squelch this message and still keep the default behaviour, set  'receive.denyCurrentBranch' configuration variable to 'refuse'.
>To file:///Users/hukar/Documents/programmation/udemy_cours_git/clone_mon_site
>! [remote rejected] master -> master (branch is currently checked out)
>error: failed to push some refs to 'file:///Users/hukar/Documents/programmation/udemy_cours_git/clone_mon_site'
>```

>en français:

>```bach
>remote: error: refusant de mettre à jour la branche extraite: refs / heads / master
>Par défaut, mise à jour de la branche actuelle dans un référentiel non-bare est refusé, car l'index et l'arborescence de travail seront incohérents par rapport à ce que vous avez poussé, et il faudra associer 'git reset --hard' l'arbre de travail à HEAD.
>Vous pouvez définir la variable de configuration 'receive.denyCurrentBranch' sur 'ignore' ou 'warn' dans le 'remote repository' pour autoriser le transfert dans sa branche actuelle; toutefois, cela n'est pas recommandé à moins que vous n'ayez prévu de mettre à jour son arbre de travail pour qu'il corresponde à ce que vous avez poussé d'une autre manière.
>Pour supprimer ce message tout en conservant le comportement par défaut, définissez la variable de configuration 'receive.denyCurrentBranch' sur 'refuser'.
>Pour déposer: /// Users / hukar / Documents / programmation / udemy_cours_git / clone_mon_site
>  ! [rejeté à distance] maître -> maître (la branche est actuellement extraite)
>erreur: échec de l'insertion de références dans 'fichier: /// Utilisateurs / hukar / Documents / programmation / udemy_cours_git / clone_mon_site'
>```


Dans le projet de départ:

On crée un remote:

```bash
git remote add titi file:///Users/hukar/Drive/hypopro-docu/mon-projet/workspace
git push titi master
```

Pour récupérer ses fichier en local on crée un clone.

`Users/hukar/Documents`
```bash
mkdir projet_local
cd projet_local
git clone file:///Users/hukar/Drive/hypopro-docu/mon-projet/workspace workspace_local
```

