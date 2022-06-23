# Node Version Manager NVM

Pour mettre à jour Node il vaut mieux passer par cet utilitaire.

https://github.com/creationix/nvm

## 1 - Faire le ménage

Voici un article **Stackoverflow** pour supprimer les traces des anciennes installations de Node :

> To completely uninstall node + npm is to do the following:
> 1. go to **/usr/local/lib** and delete any **node** and **node_modules**
> 2. go to **/usr/local/include** and delete any **node** and **node_modules** directory
> 3. if you installed with **brew install node**, then run **brew uninstall node** in your terminal
> 4. check your Home directory for any **local** or **lib** or **include** folders, and delete any **node** or **node_modules** from there
> 5. go to **/usr/local/bin** and delete any **node** executable
>
> You may also need to do:
>
> ```bash
> sudo rm -rf /opt/local/bin/node /opt/local/include/node /opt/local/lib/node_modules
> sudo rm -rf /usr/local/bin/npm /usr/local/share/man/man1/node.1 /usr/local/lib/dtrace/node.d
> ```
>
> Additionally, NVM modifies the PATH variable in `$HOME/.bashrc`, which must be [reverted manually](https://github.com/creationix/nvm#removal). (```$HOME/.profile```)
>
> Then download **nvm** and follow the instructions to install node. The latest versions of node come with **npm**, I believe, but you can also reinstall that as well.



## 2 - Installer NVM

```bash
curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.11/install.sh | bash
```

> The script clones the nvm repository to `~/.nvm` and adds the source line to your profile (`~/.bash_profile`, `~/.zshrc`, `~/.profile`, or `~/.bashrc`).

## 3 - Lister les versions de Node disponible

```bash
nvm ls-remote
```

## 4 - installer une version

```bash
nvm install 8.1.0
```

## 5- Choisir une version (parmi toutes celles installées)

```bash
nvm use 4.2
```