# 01 Startup `Nuxt`

## SSR

### Server Side Rendering

Les appels à l'`API` se font côté serveur, ce qui améliore le référencement (Search Engine Optimisation).

Les pages étant construite côté serveur aussi, la vitesse de chargement est aussi améliorée (?).



## Pre Rendering

Les fichiers `html` sont rendu en amont.

On a les bénéfice du `SSR` plus l'hébergement gratuit.



## Code Splitting

Découpe le javacsript dans plusieurs fichiers.

Un fichiers javascript ne contenant que l'essentiel est créé pour chaque page.



## Création d'un projet

```bash
yarn create nuxt-app my-app
```



## Arborescence

`assets` contient les fichiers qui ne doivent pas être compilé `image` ou `font`.

`layouts` change le style général du `layout` (`sidebar`, `appbar`).

`pages` correspond aux routes.

`plugins` enregistre une fonctionnalité globalement.

`static` automatiquement accessible à l'adresse `root` du serveur : `url/ma-ressource.png`

`store` : `vuex`