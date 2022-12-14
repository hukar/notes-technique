# 01 `Clean`  Architecture



## Différence avec une architecture `n-Tier`

<img src="assets/clean-vs-tier-architecture-schema.png" alt="clean-vs-tier-architecture-schema" style="zoom:50%;" />

L'architecture `n-Tier/n-Layer` est centrée sur la `BDD` (technologie) alors que l'architecture `Clean` est centrée sur le `Domain` (le métier).



## Architecture `N-Tier`

Cette architecture définie déjà une séparation du code :

<img src="assets/n-tier-n-layer-schema-architecture.png" alt="n-tier-n-layer-schema-architecture" style="zoom:50%;" />



## `Hexagonal`/`Ports and Adapters` Architecture

<img src="assets/hexagonal-architecture-schema.png" alt="hexagonal-architecture-schema" style="zoom:50%;" />



## `Clean` Architecture

<img src="assets/clean-architecture-schema.png" alt="clean-architecture-schema" style="zoom:50%;" />

La couche `Infrastructure ` sert à communiquer avec tous le reste :

- Base de données
- Services extérieur
- Système de mail
- ...



## Règles de la `Clean` Architecture

<img src="assets/clean-architecture-rules.png" alt="clean-architecture-rules" style="zoom:50%;" />



- Toutes les `règles métiers` et toutes les entités sont dans le `Core Project` (`Domain`)
- Toutes les dépendances pointes vers le `Core Project`
- Les projets intérieurs définissent les `interfaces`, les projets extérieurs les `implémentent`



## Composition du `Core` project

<img src="assets/inner-core-project-members.png" alt="inner-core-project-members" style="zoom:50%;" />

Je retiens :

- Les `Entities`
- Les `Services` (`CQRS`)
- Les `Interfaces`
- Les `Customs Exception`
- Les `Validators`



## Composition du `Infrastructure` Project

On va trouver ici toutes les implémentations des `interface` du `Core` Project.

Toutes les dépendences avec les librairies externe doivent être dans `Infrastructure` Project.

<img src="assets/what-belong-infrastructure-project.png" alt="what-belong-infrastructure-project" style="zoom:50%;" />

- `Repository`
- Gestion du `Cache`
- `API Client`
- `Email`, `sms`
- `File System`
- `System Clock`



## Le `Web` Project

Il peut être de différent type :

<img src="assets/what-belong-in-the-web-project.png" alt="what-belong-in-the-web-project" style="zoom:50%;" />

Les `Dtos` ou `ViewModels` sont définie ici.



## Principe du `Shared Kernel` Project

Ce sont les classes partagées par plusieurs applications.

Il n'y a pas de dépendence avec `Infrastructure`.

<img src="assets/shared-kernel-composition.png" alt="shared-kernel-composition" style="zoom:50%;" />





