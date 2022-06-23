# Clean Architecture

## Ian cooper

https://www.youtube.com/watch?v=SxJPQ5qXisw



### Le couplage

C'est une propriété qui force un module à changer parce qu'un autre module change.



### Cohesion

C'est une propriété qui oblige des modules de même sujet à changer ensemble.



### Les couches logicielles

Elles permettent moins de couplage et plus de cohesion.



## Logique métier

On peut distinguer deux types de `Business Logic` :

### 1 `Domain Logic`

Ce sont les règles appartenant purement au domaine métier :

- Le calcul des taxes, des taux
- Le calcule des délais
- Le status d'un dossier (?)



### 2 `Application Logic`

C'est ce que le système doit entreprendre pour réaliser une tâche (`Workflow Logic`) :

- Se connecter à la `DB`
- *mapper* des objets
- lire des infos dans un fichier



Le `Domain` se scinde en deux, les `Services` et les `Entities`.



## Ports et Adaptateurs

C'est en fait l'`Architecture Hexagonale`.

































