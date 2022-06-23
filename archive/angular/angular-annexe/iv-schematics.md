# Angular Schematics

Outil de création de projet, permet d'automatiser la génération de fichiers et de dossier à l'aide de template.

Pour commencer :

```bash
npm install -g @angular-devkit/schematics-cli

schematics blank --name=hello-kiki

# ou bien avec des commentaires
schematics schematic --name=hello-kiki

#puis pour loader les dépendences
cd hello-kiki
npm install
```

ce qui donne :

![](/Users/hukar/Documents/notes-techniques/cheat-sheet/img/schematic-project.png)

`index.ts` est une `factory` qui renvoie une `rule` chargée de définir le template

`index.ts` :

```typescript
import { Rule, SchematicContext, Tree } from '@angular-devkit/schematics';


// You don't have to export the function as default. You can also have more than one rule factory
// per file.
export function helloCoco(_options: any): Rule {
  return (tree: Tree, _context: SchematicContext) => {
    _context.logger.info("every body likes turttles");
    tree.create("coco.txt", "hello coco!");
    return tree;
  };
}

```

`tree.create(nomFichier: path, contenuFichier: content)`

On ajoute dans l'arbre (du projet) un fichier `coco.txt` et on écrit dedans `"every ..."`

ensuite on le *transpile* :

```bash
npm run build
```

Ensuite on lance schematics :

```bash
schematics .:hello-coco
```

affiche dans la console :

```bash
every body likes turttles
CREATE /coco.txt (11 bytes)
```

Mais rien n'est vraiment créer car par défaut on est en debug mode, il faut donc désactiver le debug pour écrire réellement dans le système de fichier:

```bash
schematics .:hello-coco --debug false
```

équivalent :

```bash
ng generate .:hello-coco
```

