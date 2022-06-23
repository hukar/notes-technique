# 01 `Axios` interceptors

## Progress Bar `NProgress`

Utilisation de la librairie `NProgress`.

```bash
npm i nprogress
```

<img src="assets/Screenshot2020-11-17at15.50.50.png" alt="Screenshot 2020-11-17 at 15.50.50" style="zoom: 25%;" />

Dans `main.js` on importe les `css` de `nprogress`

```js
// ...
import "nprogress/nprogress.css";

// ...
```

## Implémentation

Il existe trois façons d'implémenter la progress bar :

<img src="assets/Screenshot2020-11-17at15.56.22.png" alt="Screenshot 2020-11-17 at 15.56.22" style="zoom:25%;" />

## utiliser `Axios` interceptors

<img src="assets/Screenshot2020-11-17at15.58.17.png" alt="Screenshot 2020-11-17 at 15.58.17" style="zoom:33%;" />

Dans `event.js`

```js
import axios from "axios";
import NProgress from "nprogress"; // on importe NProgress

const apiClient = axios.create({
  baseURL: `http://localhost:3000`,
  withCredentials: false, // This is the default
  headers: {
    Accept: "application/json",
    "Content-Type": "application/json",
  },
});

apiClient.interceptors.request.use((config) => {
  NProgress.start();
  return config;
});

apiClient.interceptors.response.use((response) => {
  NProgress.done();
  return response;
});

export default {
  getEvents(perPage, page) {
    return apiClient.get("/events?_limit=" + perPage + "&_page=" + page);
  },
  getEvent(id) {
    return apiClient.get("/events/" + id);
  },
  postEvent(event) {
    return apiClient.post("/events", event);
  },
};
```

`axios.interceptors.request.use` et `axios.interceptors.response.use` s'utilise comme des `middleware`.

Du coup il faut retourner soit la `config` soit la `response`.

On utilise simplement `NProgresse.start()` et `NProgress.done()`.

Pour simuler une attente réseau on a `-d` qui ajoute un délai (en ms) à la réponse de `json-server`

```bash
json-server -d 2500 db.json
```

<img src="assets/Screenshot2020-11-17at16.11.16.png" alt="Screenshot 2020-11-17 at 16.11.16" style="zoom:33%;" />

On voit la barre et le loader circulaire en haut.

### Problèmes

Cette solution n'est pas optimale si il y a plusieurs `call` à l'`API`.

<img src="assets/Screenshot2020-11-17at16.13.48.png" alt="Screenshot 2020-11-17 at 16.13.48" style="zoom:33%;" />

Le `template` est chargé avant que les données ne soit arrivées.

## Intérêts des `interceptors`

Les `interceptors` restent intéressant pour

- ajouter les token d'authorisation à la requête
- formatter les données de la réponse avant de les transmettre à l'application
- pour intercepter les réponse 401 non autorisé
