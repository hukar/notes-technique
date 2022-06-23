# Intervertir les slots de déploiement

<img src="assets/Screenshot2020-07-16at11.51.53.png" alt="Screenshot 2020-07-16 at 11.51.53" style="zoom:50%;" />

Il faut cliquer sur `swap`.

Les variables d'environnement dont `Deployment slot setting` est coché ne seront pas copier lors du `swap`.

On devrait donc avoir un nouveau `MESSAGE`.

### Avant le swap

#### `staging`

<img src="assets/Screenshot2020-07-16at11.54.19.png" alt="Screenshot 2020-07-16 at 11.54.19" style="zoom:50%;" />

#### `production`

<img src="assets/Screenshot2020-07-16at11.54.43.png" alt="Screenshot 2020-07-16 at 11.54.43" style="zoom:50%;" />

### après le `swap`

#### `staging`

<img src="assets/Screenshot2020-07-16at11.58.07.png" alt="Screenshot 2020-07-16 at 11.58.07" style="zoom:50%;" />

#### `production`

<img src="assets/Screenshot2020-07-16at11.58.28.png" alt="Screenshot 2020-07-16 at 11.58.28" style="zoom:50%;" />

On voit que la variable d'environnement `MESSAGE` est restée liée à un `slots` (ici `production`).

Si on affiche aussi la variable d'environnement `NODE_ENV` qui n'est pas attachée au `slot` on obtient :

### staging

 <img src="assets/staging-slot-swap.png" alt="staging-slot-swap" style="zoom:50%;" />

`MESSAGE`  : `Deployment slot setting = true` staging (la valeur reste attachée au `slot`).

`NODE_ENV` : `Deployment slot setting = false`  production la valeur est attachée à l'application.



### `production`

<img src="assets/production-slot-swap.png" alt="production-slot-swap" style="zoom:50%;" />

Ici c'est l'inverse.