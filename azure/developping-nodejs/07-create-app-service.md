# 07 Créer un App service

## `Express-generator`

```bash
sudo npm i -g express-generator
```

```bash
express webapp
cd webapp
npm i
```

<img src="assets/Screenshot2020-07-14at11.48.50.png" alt="Screenshot 2020-07-14 at 11.48.50" style="zoom:50%;" />

## Ouvrir dans VSCode et créer un dépôt `GIT`

Utilisation du plugin VSCode `gitignore template` pour générer un fichier `.gitignore`.

<img src="assets/Screenshot2020-07-14at11.57.09.png" alt="Screenshot 2020-07-14 at 11.57.09" style="zoom: 50%;" />

Faire un `commit` initiale.

## Création de `App Services`

<img src="assets/Screenshot2020-07-14at14.58.58.png" alt="Screenshot 2020-07-14 at 14.58.58" style="zoom:50%;" />

On doit toujours créer un groupe de ressource.

<img src="assets/Screenshot2020-07-14at15.02.52.png" alt="Screenshot 2020-07-14 at 15.02.52" style="zoom:50%;" />

Le nom doit être unique globalement.

<img src="assets/instance-details.png" alt="instance-details" style="zoom:50%;" />

### App Service Plan

<img src="assets/Screenshot2020-07-14at15.07.17.png" alt="Screenshot 2020-07-14 at 15.07.17" style="zoom:50%;" />

Va définir les ressources et les fonctionnalités disponible pour notre application.

<img src="assets/Screenshot2020-07-14at15.09.39.png" alt="Screenshot 2020-07-14 at 15.09.39" style="zoom:50%;" />

### Niveau de prix

<img src="assets/Screenshot2020-07-14at15.11.24.png" alt="Screenshot 2020-07-14 at 15.11.24" style="zoom:50%;" />

L'hébergement partagé est a évité en production, il sert aux tests et aux développement.

### Mise à l'échelle : scaling

<img src="assets/Screenshot2020-07-14at15.14.33.png" alt="Screenshot 2020-07-14 at 15.14.33" style="zoom:50%;" />

`vertical scaling` augmenter l'instance elle-même dans ses capacités.

`horizontal scaling` augmenter le nombre d'instance.

### Azure Compute Unit `ACU`

<img src="assets/Screenshot2020-07-14at15.19.19.png" alt="Screenshot 2020-07-14 at 15.19.19" style="zoom:50%;" />

C'est une valeur (unité) de puissance relative à une configuration de référence.

### `SKU`

Il signifie «Stock-Keeping Unit». Extrait des [documents Microsoft](https://docs.microsoft.com/en-us/partner-center/develop/product-resources#sku) :

> # Sku
>
> Représente une unité de stockage (SKU) achetable sous un produit. Ceux-ci représentent les différentes formes du produit.

Cela semble conforme à la [définition de Wikipedia](https://en.wikipedia.org/wiki/Stock_keeping_unit) :

> Dans le domaine de la gestion des stocks, une unité de stockage est un type distinct d'article à vendre.

On pourrait penser aux déclinaisons du produit.

Un SKU est un produit unique et le plus précis possible.

### Choix de l'App Service Plan

<img src="assets/Screenshot2020-07-14at16.11.54.png" alt="Screenshot 2020-07-14 at 16.11.54" style="zoom:50%;" />

On donne un nom à son plan.

<img src="assets/Screenshot2020-07-14at16.12.11.png" alt="Screenshot 2020-07-14 at 16.12.11" style="zoom:50%;" />

On choisie le S1 qui a des fonctionnalités en plus par rapport à un plan `Dev/Test` dont `Custom domains`.

`Review + create` et enfin `create`.

<img src="assets/Screenshot2020-07-14at16.18.02.png" alt="Screenshot 2020-07-14 at 16.18.02" style="zoom:50%;" />

`Go to resource` :

<img src="assets/Screenshot2020-07-14at16.18.38.png" alt="Screenshot 2020-07-14 at 16.18.38" style="zoom:50%;" />

On voit l'`URL` de notre application

`https://hukar-weapp-nodejs.azurewebsites.net`
