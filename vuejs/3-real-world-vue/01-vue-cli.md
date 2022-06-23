#  01 Vue CLI

```bash
npm i -g @vue/cli

vue create my-new-vue-project
```

<img src="assets/Screenshot2020-09-24at14.35.35.png" alt="Screenshot 2020-09-24 at 14.35.35" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at14.36.56.png" alt="Screenshot 2020-09-24 at 14.36.56" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at14.37.39.png" alt="Screenshot 2020-09-24 at 14.37.39" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at14.39.17.png" alt="Screenshot 2020-09-24 at 14.39.17" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at14.40.07.png" alt="Screenshot 2020-09-24 at 14.40.07" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at14.40.40.png" alt="Screenshot 2020-09-24 at 14.40.40" style="zoom:33%;" />

```bash
npm run serve
```

<img src="assets/Screenshot2020-09-24at14.47.08.png" alt="Screenshot 2020-09-24 at 14.47.08" style="zoom:33%;" />

## `Vue UI`

On peut faire ça aussi avec l'interface graphique `Vue UI`.

```bash
vue ui
```

<img src="assets/Screenshot2020-09-24at14.50.43.png" alt="Screenshot 2020-09-24 at 14.50.43" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at15.03.41.png" alt="Screenshot 2020-09-24 at 15.03.41" style="zoom:33%;" />

<img src="assets/Screenshot2020-09-24at15.04.39.png" alt="Screenshot 2020-09-24 at 15.04.39" style="zoom:33%;" />

## arborescence

`public` image, fichiers ne devant pas être calculé par `webpack`.

`src` le code spécifique à l'application.

 `assets` images, polices, etc.

 `components` blocs de construction de l'application `Vue`.

 `views` différentes pages (ou vue) de l'application.

 `App.vue` le composant `root`.

 `main.js` rendue de l'application, les composants sont montés dans le DOM.

 `router.js` le routeur de `Vue`.

 `store.js` pour `Vuex`.

L'application est montée dans `public/index.html` :

```html
	<!-- ... -->
	<div id="app"></div>
    <!-- built files will be auto injected -->
  </body>
</html>
```

`src/main.js`

```js
import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import store from "./store";

Vue.config.productionTip = false;

new Vue({
  router,
  store,
  render: (h) => h(App),
}).$mount("#app");
```

<img src="assets/Screenshot2020-11-04at14.56.47.png" alt="Screenshot 2020-11-04 at 14.56.47" style="zoom:33%;" />

`App.vue` est un conteneur pour les composants.

## Build

On a dans `index.html` ce message : `<!-- built files will be auto injected -->`

Lançons un `build` :

```bash
npm run build
```

<img src="assets/Screenshot2020-11-04at15.00.18.png" alt="Screenshot 2020-11-04 at 15.00.18" style="zoom:33%;" />

Un dossier `dist` est construit (`distribution`), c'est l'application pour la production (pour le déploiement).

`dist/index.html`

```html
        <div id="app"></div>
        <script src="/js/chunk-vendors.128c4178.js"></script>
        <script src="/js/app.3139c9d2.js"></script>
	</body>
</html>
```

`<script src="/js/chunk-vendors.128c4178.js"></script>` contient les dépendances.

`<script src="/js/app.3139c9d2.js"></script>` contient l'application depuis `main.js`.
