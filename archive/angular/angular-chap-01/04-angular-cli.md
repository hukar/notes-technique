#04 Angular CLI

## Installer angular-cli

```sh
npm install -g @angular/cli
```



##créer un nouveau projet

```shell
ng new my-firts-app
```



## le faire tourner sur un serveur

```shell
cd my-first-app
ng serve # port 4200
```



## Générer un composant

```shell
ng generate component servers #ou bien
ng g c servers #forme raccourci
```

```shell
# résultat généré automatiquement
CREATE src/app/servers/servers.component.html (26 bytes)
CREATE src/app/servers/servers.component.spec.ts (635 bytes) # pour les tests
CREATE src/app/servers/servers.component.ts (273 bytes)
CREATE src/app/servers/servers.component.css (0 bytes)
UPDATE src/app/app.module.ts (556 bytes)
```

Sans le fichier de test:

```sh
ng g c recipes --spec false
```



## créer un directive avec le CLI

```sh
ng generate directive ma-directive
# ou bien
ng g d ma-directive
```

