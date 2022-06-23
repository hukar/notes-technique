# 03 Router

## anatomie

<img src="assets/Screenshot2020-11-05at14.23.54.png" alt="Screenshot 2020-11-05 at 14.23.54" style="zoom:33%;" />

Le fichier `router.js` permet de configurer les routes.

<img src="assets/Screenshot2020-09-24at16.42.43.png" alt="Screenshot 2020-09-24 at 16.42.43" style="zoom:33%;" />

Les composants utilisé par le routeur sont mis dans un dossier `views` mais il reste des composants.

Si un composant est chargé par le routeur alors on le met dans `views`. (Bonne pratique)

On pourrait aussi appeler ce dossier `pages` par exemple.

## `<router-link to="Route" >`

 <img src="assets/Screenshot2020-11-05at14.33.06.png" alt="Screenshot 2020-11-05 at 14.33.06" style="zoom:33%;" />

`<router-link>` crée un lien pour naviguer vers une page.

`<router-view>` est l'endroit où la vue sera rendue.

## Création d'une route dans le routeur

```js
import Vue from "vue";
import VueRouter from "vue-router";
import Home from "../views/Home.vue";
import Custom from "../views/Custom.vue";

Vue.use(VueRouter);

const routes = [
  {
    path: "/",
    name: "Home",
    component: Home,
  },
  {
    path: "/custom",
    name: "Custom",
    component: Custom,
  },
];

const router = new VueRouter({
  routes,
});

export default router;
```

Dans le template :

```html
<div id="nav">
  <router-link to="/">Home</router-link> |
  <router-link to="/about">About</router-link> |
  <router-link to="/custom">Custom</router-link>
</div>

<router-view />
<!-- endroit ou le composant est chargé -->
```

Avec l'extension `Vue` des `Developer tools` :

<img src="assets/Screenshot2020-11-05at15.46.55.png" alt="Screenshot 2020-11-05 at 15.46.55" style="zoom:33%;" />

### lazy loading

Génère un morceau de code séparé au `build` :

<img src="assets/Screenshot2020-09-25at10.43.51.png" alt="Screenshot 2020-09-25 at 10.43.51" style="zoom:33%;" />

Dans `router/index.js`

```js
{
    path: '/about',
    name: 'About',
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component: () =>
        import(/* webpackChunkName: "about" */ '../views/About.vue')
}
```

Du coup on a une requête supplémentaire :

<img src="assets/Screenshot2020-09-25at10.49.28.png" alt="Screenshot 2020-09-25 at 10.49.28" style="zoom:50%;" />

Mais cette requête n'intervient qu'une seule fois.

- Si la page n'est jamais demandée, on télécharge moins de code.

- Si la page est demandé, la première fois un fichier complémentaire est téléchargé, puis plus de téléchargement par la suite.

## Navigation avec le `name`

On voit qu'un attribut `name` est renseigné, on peut l'utiliser à la place du `path` :

```html
<div id="app">
  <div id="nav">
    <router-link :to="{ name: 'Home' }">Home</router-link> |
    <router-link :to="{ name: 'About' }">About</router-link> |
    <router-link :to="{ name: 'Custom' }">Custom</router-link>
  </div>
  <router-view />
</div>
```

Cela permet si le `path` change, de ne pas changer les liens du `template`.

<img src="assets/Screenshot2020-09-25at11.00.42.png" alt="Screenshot 2020-09-25 at 11.00.42" style="zoom:33%;" />

## Redirection : `redirect`

```js
const routes = [
    {
        path: '/',
        name: 'Home',
        component: Home
    },
    {
        path: '/custom',
        name: 'Custom',
        component: Custom
    },
    {
        path: '/product-unknow',
        redirect: { name: 'Custom' }
    },
```

ou bien :

```js
	{
        path: '/product-unknow',
        redirect: '/custom'
    },
```

### `alias`

On peut aussi utiliser un alias :

```js
const routes = [
  {
    path: "/",
    name: "Home",
    component: Home,
  },
  {
    path: "/about-us",
    name: "About",
    component: About,
    alias: "/about",
  },
];
```

Mais ici on a deux `url` qui pointent vers la même page :

<img src="assets/Screenshot2020-11-05at15.55.34.png" alt="Screenshot 2020-11-05 at 15.55.34" style="zoom:33%;" />

<img src="assets/Screenshot2020-11-05at15.55.50.png" alt="Screenshot 2020-11-05 at 15.55.50" style="zoom:33%;" />
