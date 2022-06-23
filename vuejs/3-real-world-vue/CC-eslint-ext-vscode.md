# CC L'extension `eslint vscode`

Intègre [ESLint](http://eslint.org/) dans code VS. Si vous êtes nouveau à `ESLint`, consultez la [documentation](http://eslint.org/).

L'extension utilise la bibliothèque `ESLint` installée dans le dossier de l'espace de travail ouvert. 

Si le dossier n'en fournit pas, l'extension recherche une version d'installation **globale**. 

Si vous n'avez pas installé ESLint localement ou globalement, faites-le en lançant `npm install eslint` dans le dossier `workspace` pour une installation locale ou `npm install -g eslint` pour une installation globale.

Pour les nouveaux dossiers, vous devrez peut-être aussi créer un fichier de configuration `.eslintrc`

Vous pouvez le faire en utilisant la commande VS Code `Create ESLint configuration` ou en lançant la commande `eslint` dans un terminal. 

Si vous avez installé ESLint globalement (voir ci-dessus), exécutez alors la commande [`eslint --init`](http://eslint.org/docs/user-guide/command-line-interface) dans un terminal. 

Si vous avez installé ESLint localement, exécutez [`.\node_modules\.bin\eslint --init`](http://eslint.org/docs/user-guide/command-line-interface) sous Windows et [`./node_modules/.bin/eslint --init`](http://eslint.org/docs/user-guide/command-line-interface) sous Linux et Mac.

## Options ext VSCode

- ESLint peut maintenant être utilisé comme formateur. Pour activer cette fonctionnalité, utilisez le paramètre `eslint.format.enable`.
- Amélioration de la correction automatique lors de l'enregistrement - La correction automatique lors de l'enregistrement fait maintenant partie `VS Code's Code Action on Save infrastructure` et calcule toutes les corrections possibles en un seul tour.
	Elle est personnalisée via le paramètre `editor.codeActionsOnSave` . 
	 Le paramètre supporte la propriété spécifique à ESLint `source.fixAll.eslint`. 
	L'extension respecte également la propriété générique `source.fixAll`.

Le paramètre ci-dessous active la correction automatique pour tous les fournisseurs, y compris `ESLint` :

```json
    "editor.codeActionsOnSave": {
        "source.fixAll": true
    }
```

En revanche, cette configuration ne l'active que pour `ESLint` :

```json
    "editor.codeActionsOnSave": {
        "source.fixAll.eslint": true
    }
```

Vous pouvez également désactiver ESLint de manière sélective via :

```json
    "editor.codeActionsOnSave": {
        "source.fixAll": true,
        "source.fixAll.eslint": false
    }
```

Notez également qu'il y a un budget temps de `750ms` pour exécuter des actions de code sur la sauvegarde qui pourrait ne pas être suffisant pour un gros fichier JavaScript / TypeScript. Vous pouvez augmenter le budget temps en utilisant le paramètre `editor.codeActionsOnSaveTimeout`.

L'ancien paramètre `eslint.autoFixOnSave` est maintenant obsolète et peut être supprimé en toute sécurité.

## Options de paramétrage

Cette extension apporte les variables suivantes aux [paramètres](https://code.visualstudio.com/docs/customization/userandworkspace) :

- `eslint.enable` : active/désactive ESLint. Est activé par défaut.

- `eslint.debug` : active le mode de débogage d'ESLint (identique à l'option de ligne de commande --debug). Veuillez consulter le canal de sortie d'ESLint pour la sortie de débogage. Cette option est très utile pour traquer les problèmes de configuration et d'installation d'ESLint car elle fournit des informations verbeuses sur la manière dont ESLint valide un fichier.

- `eslint.lintTask.enable` : si l'extension contribue à une tâche de lint pour lint un dossier entier de l'espace de travail.

- Options de l'extension `eslint.lintTask.options` : Options de la ligne de commande appliquées lors de l'exécution de la tâche pour linturer tout l'espace de travail (https://eslint.org/docs/user-guide/command-line-interface). Un exemple pour pointer vers un fichier personnalisé ".eslintrc.json" et un ".eslintignore" personnalisé est :

```json
  {
    "eslint.lintTask.options" : "-c C:/mydirectory/.eslintrc.json --ignore-path C:/mydirectory/.eslintignore .
  }
```

- `eslint.packageManager` : contrôle le gestionnaire de paquets à utiliser pour résoudre la bibliothèque ESLint. Cela n'a d'influence que si la bibliothèque ESLint est résolue globalement. Les valeurs valides sont `"npm"` ou `"yarn"` ou `"pnpm"`.

- `eslint.options` : options permettant de configurer la façon dont ESLint est lancé en utilisant l'API du moteur CLI ESLint (http://eslint.org/docs/developer-guide/nodejs-api#cliengine). La valeur par défaut est un paquet d'options vide. Un exemple pour pointer vers un fichier `.eslintrc.json` personnalisé est :

```json
  {
    "eslint.options" : {"configFile" : "C:/mydirectory/.eslintrc.json" }
  }
```

- `eslint.run` - lancez le linter `onSave` ou `onType`, la valeur par défaut est `onType`.

- `eslint.quiet` - ignore les avertissements.

- `eslint.runtime` - utilisez ce paramètre pour définir le chemin du noeud runtime sous lequel ESLint doit être exécuté.

- `eslint.nodeEnv` - utilisez ce paramètre si un plugin ESLint ou une configuration a besoin de `process.env.NODE_ENV` pour être défini.

- `eslint.nodePath` - utilisez ce paramètre si un paquet ESLint installé ne peut pas être détecté, par exemple `/myGlobalNodePackages/node_modules`.

- `eslint.probe` = un tableau pour les identificateurs de langue pour lesquels l'extension ESLint doit être activée et doit essayer de valider le fichier. En cas d'échec de la validation pour les langues étudiées, l'extension dit "silent". Les valeurs par défaut sont `["javascript", "javascriptreact", "typescript", "typescriptreact", "html", "vue"]`.

- `eslint.validate` - un tableau d'identificateurs de langue spécifiant les fichiers pour lesquels la validation doit être appliquée. Il s'agit d'un ancien paramètre qui ne devrait plus être nécessaire dans des cas normaux. La valeur par défaut est `["javascript", "javascriptreact"]`.

- `eslint.format.enable` : active ESLint comme formateur pour les fichiers validés. Bien que vous puissiez également utiliser le formateur lors de l'enregistrement en utilisant le paramètre `editor.formatOnSave`, il est recommandé d'utiliser la fonction `editor.codeActionsOnSave` car elle permet une meilleure configurabilité.

- `eslint.workingDirectories` - spécifie comment les répertoires de travail utilisés par ESLint sont calculés. ESLint résout les fichiers de configuration (par exemple `eslintrc`, `.eslintignore`) relatifs à un répertoire de travail, il est donc important de le configurer correctement. Si l'exécution d'ESLint dans le terminal vous oblige à changer le répertoire de travail dans le terminal en un sous-répertoire, il est généralement nécessaire de modifier ce paramètre. (voir aussi [CLIEngine options#cwd](https://eslint.org/docs/developer-guide/nodejs-api#cliengine)). Veuillez également garder à l'esprit que le fichier `.eslintrc*` est résolu en tenant compte des répertoires parents alors que le fichier `.eslintignore` n'est honoré que dans le répertoire de travail actuel. Les valeurs suivantes peuvent être utilisées :

	- `[{"mode" : "location" }]` (@since 2.0.0) : indique à ESLint d'utiliser l'emplacement du dossier de l'espace de travail ou l'emplacement du fichier (si aucun dossier de l'espace de travail n'est ouvert) comme répertoire de travail. C'est la stratégie par défaut et la même que celle utilisée dans les anciennes versions de l'extension ESLint (versions 1.9.x).

	- `[{"mode" : "auto" }]` (@since 2.0.0) : indique à ESLint de déduire un répertoire de travail basé sur l'emplacement des fichiers `package.json`, `.eslintignore` et `.eslintrc*`. Cela peut fonctionner dans de nombreux cas, mais peut également conduire à des résultats inattendus.

	- `string[]` : un ensemble de répertoires de travail à utiliser. Considérez la disposition suivante des répertoires :

		```
		racine /
		client/
		  .eslintrc.json
		  client.js
		serveur/
		  .eslintignore
		  .eslintrc.json
		  server.js
		```

		Ensuite, en utilisant le réglage :

		```javascript
		"eslint.workingDirectories" : [ "./client", "./serveur" ]
		```

		validera les fichiers à l'intérieur du répertoire du serveur avec le répertoire du serveur comme répertoire de travail eslint actuel. Même chose pour les fichiers dans le répertoire client. L'extension ESLint modifiera également le répertoire de travail du processus en fonction des répertoires fournis. Si cela n'est pas souhaité, un littéral avec la propriété `!cwd` peut être utilisé (par exemple `{"directory" : "./client", "!cwd" : true }`). Le répertoire client sera alors utilisé comme répertoire de travail ESLint, mais ne modifiera pas le répertoire de travail du processus.
		`{"pattern" : glob pattern }` (@since 2.0.0) : Permet de spécifier un modèle pour détecter le répertoire de travail. Il s'agit en fait d'un raccourci pour lister tous les répertoires. Si vous avez un dépôt mono avec tous vos projets se trouvant sous un dossier packages, vous pouvez utiliser `{"pattern" : "./packages/*/" }` pour faire de tous ces dossiers des répertoires de travail.





- `eslint.codeAction.disableRuleComment` - objet avec propriétés :

	- `enable` - affiche la règle de désactivation dans le `quick fix menu`. `true` par défaut.

	- `location` - choisir d'ajouter le commentaire `eslint-disable` sur la `separateLine` ou la `sameLine`. `separateLine` est la valeur par défaut. Exemple :

		```json
		{ "enable" : true, "location" : "sameLine" }
		```

- `eslint.codeAction.showDocumentation` - objet avec des propriétés :

	- `enable` - affiche la page web de documentation dans le menu `quick fix`. `true` par défaut.

- `eslint.codeActionsOnSave.mode` (@since 2.0.12) : contrôle les problèmes qui sont corrigés lors de l'exécution d'actions de code lors de la sauvegarde

	- `all` : corrige tous les problèmes possibles en revalidant le contenu du fichier. Cela exécute le même chemin de code que l'exécution d'eslint avec l'option 
	- `fix` dans le terminal et peut donc prendre un certain temps. C'est la valeur par défaut.
	- `problems` : ne corrige que les problèmes réparables actuellement connus tant que leurs éditions textuelles ne se chevauchent pas. Ce mode est beaucoup plus rapide mais ne corrige probablement qu'une partie des problèmes.

- `eslint.format.enable` (@since 2.0.0) : utilise ESlint comme formateur pour les fichiers qui sont validés par ESLint. Si cette option est activée, veillez à désactiver les autres formateurs si vous voulez que ce soit la valeur par défaut. Une bonne façon de le faire est d'ajouter le paramètre suivant `"[javascript]" : "[editor.defaultFormatter]" : "dbaeumer.vscode-eslint" }` pour JavaScript. Pour TypeScript, vous devez ajouter `"[typescript]" : {"editor.defaultFormatter" : "dbaeumer.vscode-eslint" }`.

- `eslint.onIgnoredFiles` (@since 2.0.10) : utilisé pour contrôler si des avertissements doivent être générés lorsque l'on tente de lier des fichiers ignorés. La valeur par défaut est `off`. Peut être réglé sur `warn`.

- `editor.codeActionsOnSave` (@since 2.0.0) : ce paramètre supporte maintenant une entrée `source.fixAll.eslint`. S'il est défini à `true`, toutes les erreurs ESLint auto-fixables de tous les plugins seront corrigées lors de la sauvegarde. Vous pouvez également activer et désactiver de manière sélective des langues spécifiques en utilisant les paramètres de sélection de langue de VS Code. Pour désactiver codeActionsOnSave pour les fichiers HTML, utilisez le paramètre suivant :

```json
  "[html]" : {
    "editor.codeActionsOnSave" : {
      "source.fixAll.eslint" : false
    }
  }
```

L'ancien paramètre `eslint.autoFixOnSave` est désormais obsolète et peut être supprimé en toute sécurité.

 Veuillez également noter que si vous utilisez ESLint comme formatteur par défaut, vous devez désactiver `editor.formatOnSave` lorsque vous avez activé `editor.codeActionsOnSave`. Sinon, votre fichier sera corrigé deux fois, ce qui est inutile.

## Commands:

Cette extension apporte les commandes suivantes à la palette de commandes.

- `Create '.eslintrc.json' file`: crée un nouveau fichier `.eslintrc.json`.
- `Fix all auto-fixable problems`: applique la régle ESLint `auto-fix` pour fixer tous les problèmes.
- `Disable ESLint for this Workspace`: désactive l'extension ESLint pour cet espace de travail.
- `Enable ESLint for this Workspace`: enable ESLint extension for this workspace.
- `Reset Library Decisions`: Resets the ESLint library validation confirmations.
- `Manage Library Execution`: Opens the library execution confirmation dialog.

## Utiliser l'extension avec la tâche de VS Code en cours d'exécution

L'extension *lint* un fichier individuel à la frappe seulement. Si vous voulez lier tout l'espace de travail, mettez `eslint.lintTask.enable` à `true` et l'extension contribuera également à la tâche `eslint : lint whole folder`. Il n'est plus nécessaire de définir une tâche personnalisée dans `tasks.json`.

