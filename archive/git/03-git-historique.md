# 03 git historique

Anatomie d'un commit :

```js
f8725f2bfc75aa875988b4b88944ae0f5e7ee7c1 // SHA-1 pointeur commit parent
6dbe1c1e2f30f87328c005361a9b1b934df8dc33 // SHA-1 de ce commit
Karim Meshoub <k.meshoub@gmail.com> // info de config
1541251206 +0100  // date timestamp correspondant à 03/11/2018 à 14:20:06.	
commit: un commit en vacances // le message du commit
```

informations trouvées dans `.git/logs/HEAD`.

**SHA** => Secure Hach Algorithm : hachage cryptographique

Chaque commit possède une liste de pointeur sur un ou plusieurs parent.

## tag

intitulé + un pointeur (SHA-1)

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-tag.png)

Le dernier commit possède le tag ***master***.

Le tag ***master*** se déplace en même temps que les commit.

On peut créer ses propres tag, mais ils resteront attachés à leurs commit.

Git gère aussi un autre pointeur HEAD qui indique le commit sur lequel nous sommes actuellement.

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/git-pointeur-head.png)

## Voire les commit `git log`

```bash
git log
```

```bash
commit 6dbe1c1e2f30f87328c005361a9b1b934df8dc33 (HEAD -> master)
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 14:20:06 2018 +0100

    un commit en vacances

commit f8725f2bfc75aa875988b4b88944ae0f5e7ee7c1
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 13:11:54 2018 +0100

    un koala ajouté

commit 4b5031171ec27afb678dadc49fe2ff6dd1a4c384
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 12:45:17 2018 +0100

    mon premier commit
```

On voit que ***HEAD*** est positionné sur ***master***.

### Montrer un nombre réduit de commit `git log -n`

Si on veut montrer seulement les deux dernier commit :

```bash
git log -n 2
```

```bash
commit 6dbe1c1e2f30f87328c005361a9b1b934df8dc33 (HEAD -> master)
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 14:20:06 2018 +0100

    un commit en vacances

commit f8725f2bfc75aa875988b4b88944ae0f5e7ee7c1
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 13:11:54 2018 +0100

    un koala ajouté
```

### Afficher les modification `git show <SHA-1>`

Pour afficher le détail des modifications :

```bash
git show 6dbe1c1e2f30f87328c005361a9b1b934df8dc33
```

```bash
diff --git a/hello.html b/hello.html
index b88521b..39b50bc 100644
--- a/hello.html
+++ b/hello.html
@@ -16,6 +16,9 @@
                <div id="top-banner">
                        Bienvenue dans cette formation GIT !
                </div>
+               <div>^M
+                       <p>Un singe vaut mieux que deux koala</p>^M
+               </div>^M
                
        </body>
 </html>
\ No newline at end of file
```

On retrouve l'affichage que l'on aurait eu avec `diff`.

On peut aussi utiliser les tag à la place des `SHA-1` :

```bash
git show master
```

### ! Après un show pour sortir devant `:` taper `q`.

## Se déplacer dans l'historique `git checkout`

```bash
git checkout 4b784ea86de6296bc9eb16b7d374366235800dde
git log
```

```bash
commit 4b784ea86de6296bc9eb16b7d374366235800dde (HEAD)
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 13:08:52 2018 +0100

    salut les copains

commit 293657f9314a16b3b9f9282594f045a07a33be70
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 12:50:13 2018 +0100
```

`git log` affiche seulement les `commit` avant le `HEAD`.

## Revenir sur le dernier état `git checkout master`

`master`  est notre branche principale.

Toutes les modifications ***commitées*** forment une branche parallèle et accessible pour l'instant seulement avec le `SHA-1`

## Ajouter un tag `git tag`

Un `tag` est un alias donné à un `SHA-1`.

On se rend d'abord sur le commit choisi :

```bash
git checkout 4b784ea86de6296bc9eb16b7d374366235800dde
```

Puis on lui ajoute un tag :

```bash
git tag copain
git log
```

```bash
commit 4b784ea86de6296bc9eb16b7d374366235800dde (HEAD, tag: copain)
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 13:08:52 2018 +0100

    salut les copains
```

On voit que `git`  a ajouté un `tag: copain`.

## Supprimer un tag `git tag —delete`

```bash
git tag --delete copain
```

Le tag est supprimé.

Maintenant on en recrée un avec un message :

```bash
git tag COP_V_1 -m "mon nouveau super tag"
git log
```

```bash
commit 4b784ea86de6296bc9eb16b7d374366235800dde (HEAD, tag: COP_V_1)
Author: Karim Meshoub <k.meshoub@gmail.com>
Date:   Sat Nov 3 13:08:52 2018 +0100

    salut les copains
```

## Lister les tag `git tag`

## Aller sur un tag :

```bash
git checkout COP_V_1
```

