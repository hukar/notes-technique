# 05 Zone de sauvegarde des modifications `stash` (planque, cachette)

Si on cherche à faire un `checkout` alors qu'il y a des modifications dans l'espace de travail, on obtient une erreur de Git :

```bash
git checkout v3_site
error: Your local changes to the following files would be overwritten by checkout:
        .gitignore
Please commit your changes or stash them before you switch branches.
Aborting
```

> erreur: vos modifications locales sur les fichiers suivants seraient écrasées par le `checkout`:
>
>  	.gitignore 
>
> Veuillez `commit` (valider)  vos modifications ou `stash` (les stocker) avant de changer de branche.
>
> Annuler

## Mettre les modifications dans la zone de `stash`

#### `git stash save [texte de ses modifications]`

```bash
git stash save "modification de .gitignore"
Saved working directory and index state On master: modification de .gitignore
```

## Lister les modifications misent en zone de `stash`

#### `git stash list`

```bash
git stash list
stash@{0}: On master: modification de .gitignore
```

Entre accolade c'est l'index de l'enregistrement en `stash`

## Voire le contenu du `stash`

#### `git stash show [index]`

```bash
git stash show 0
 .gitignore | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)
```

## Réaliser le `checkout`

Il est mainbtenant possible d'effectuer le `checkout` :

```bash
git checkout v3_site
```

Puis de revenir sur `master`

```bash
git checkout master
```

On se retrouve avant les modifications placées en zone de `stash`

Pour retrouver ses modification :

#### `git stash pop [index]`

```bash
git stash pop 0
```

Nous revoilà au point de départ.