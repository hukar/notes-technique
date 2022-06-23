# 03 Architecture d'une application Vue JS

## Arborescence

- `public` image et fichiers ne devant pas être calculés par `Webpack`.
	- `index.html` l'application est montée ici.

- `src` le code de l'application, là où le développeur travaille.
	- `assets` images, polices, ...
	- `components` blocks constitutif de l'application.
	- `views` les différentes pages de l'application.
	- `App.vue` le composant `$root`.
	- `main.js` point de montage de l'application dans le `DOM`.
	- `store.js` pour `vuex`.

