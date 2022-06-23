#00 Mettre à jour Angular/cli

Copie d'un article à cette url :

https://www.npmjs.com/package/@angular/cli#updating-angular-cli

To update Angular CLI to a new version, you must update both the global package and your project's local package.

Global package:

```bash
npm uninstall -g @angular/cli
npm cache verify
# if npm version is < 5 then use `npm cache clean` 
npm install -g @angular/cli@latest
```

Local project package:

```bash
rm -rf node_modules dist # use rmdir /S/Q node_modules dist in Windows Command Prompt; use rm -r -fo node_modules,dist in Windows PowerShell 
npm install --save-dev @angular/cli@latest
npm install
```

