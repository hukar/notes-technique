# Organisation  `fonctionnel`



## `Screaming` architecture 

> L'architecture devrait crier l'intention du système
>
> Uncle Bob 

L'application est organisée en suivant les `Use Cases` du système.

Les `Use cases` étant les interactions possible d'un utilisateur sur le système:

- Récupérer la liste des clients
- Acheter un produit
- Payer un vendeur

## (`functionnal` vs `Categorical` organisation)

<img src="assets/use-cases-vs-componants.png" alt="use-cases-vs-componants" style="zoom:50%;" />

On peut représenter une architecture par ses fonctions ou par ses constituants.

La vision par fonction (`Bedroom`, `Kitchen`, `Entry`, ...) est plus parlante sur l'utilisation de l'architecture que la liste des constituants.

On peut retrouver cette analogie dans l'organisation des dossiers d'une application.

<img src="assets/mvc-vs-use-cases-schema.png" alt="mvc-vs-use-cases-schema" style="zoom:50%;" />

<img src="assets/mvc-folders-vs-functionnal-folder-naming.png" alt="mvc-folders-vs-functionnal-folder-naming" style="zoom:50%;" />

### `Pros`

- Les éléments travaillant ensemble doivent être rassemblés ensemble.

- Il est plus facile de naviguer dans une structure basée sur les `Use Case` que sur les types d'éléments.
- Evite les limitations de tel ou tel framework



### `Cons`

- On perd les conventions du framework
- On perd la structure automatique de base
- `Categorical` est plus facile en premier lieu



## Mise en pratique

Chaque couche peut être organisée en `Functionnal` plutôt qu'en `Categorical`.

### La couche `Presentation`

<img src="assets/layer-presentation-functionnal-organisation.png" alt="layer-presentation-functionnal-organisation" style="zoom: 67%;" />

On retrouve la découpe `Customers`, `Employees`, `Products`, `Sales`.

### La couche `Application`

<img src="assets/application-layer-fucntionnal-organisation.png" alt="application-layer-fucntionnal-organisation" style="zoom:67%;" />

On retrouve nos `Use Cases` séparés en `Queries` et `Commands`.

<img src="assets/functionnal-application-cqrs-organisation.png" alt="functionnal-application-cqrs-organisation" style="zoom:67%;" />

De même pour les autres couches. Le système est ainsi facile à comprendre et `scream` ses intentions fonctionnelles.

<img src="assets/functionnal-organization-per-layer.png" alt="functionnal-organization-per-layer" style="zoom:50%;" />















