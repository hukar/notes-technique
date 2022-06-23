# 04 Distribution globale des données

<img src="assets/Screenshot2020-07-28at10.55.15.png" alt="Screenshot 2020-07-28 at 10.55.15" style="zoom:50%;" />

La réplication des données à travers le monde permet de meilleurs performance (les données données sont plus proches des utilisateurs), mais aussi la possibilité de récupérer ses données en cas de désastre dans un data-center.

## Global distribution

<img src="assets/Screenshot2020-07-28at10.58.08.png" alt="Screenshot 2020-07-28 at 10.58.08" style="zoom:50%;" />

Certaine régions ne permettent pas la `global distribution` pour des raisons géo-politique comme l'Allemagne ou la Chine.

<img src="assets/Screenshot2020-07-28at10.59.10-5926855.png" alt="Screenshot 2020-07-28 at 10.59.10" style="zoom:50%;" />

`Multi-master` est une option qui permet de rendre global à la fois la lecture et l'écriture (au lieu de seulement l'écriture).

## Test avec une application éloignée de la `DB`

Si le coût en `RUs` reste le même, la latence par contre augmente drastiquement si l'application et le `DB` sont dans des régions éloignées.

<img src="assets/Screenshot2020-07-28at11.17.02.png" alt="Screenshot 2020-07-28 at 11.17.02" style="zoom:50%;" />

Apparemment ces options ne sont pas disponible pour ma subscription.

<img src="assets/Screenshot2020-07-28at11.19.01.png" alt="Screenshot 2020-07-28 at 11.19.01" style="zoom:50%;" />

Voila ce qu'on pourrai avoir.

`Cosmos DB` bascule (`failover`) automatiquement sur la région la plus proche en cas de problème.

## Résolution des conflits

Si l'option `multi-master` permet de lire et écrire rapidement à travers le monde, il peut provoquer des conflits si la réplication et plus lente que l'écriture de nouvelles données.

<img src="assets/Screenshot2020-07-28at11.24.15.png" alt="Screenshot 2020-07-28 at 11.24.15" style="zoom:33%;" />

`Cosmos DB` stocke automatiquement le `timestamp` comme propriété :

```json
{
  // ...
  "_ts": 1595692322
}
```

### 1 Le plus récent gagne

<img src="assets/Screenshot2020-07-28at11.30.39.png" alt="Screenshot 2020-07-28 at 11.30.39" style="zoom:50%;" />

### 2 merge procédure

<img src="assets/Screenshot2020-07-28at11.29.56.png" alt="Screenshot 2020-07-28 at 11.29.56" style="zoom:50%;" />

On utilise les procédures stockées pour résoudre le conflit.

<img src="assets/Screenshot2020-07-28at11.33.28.png" alt="Screenshot 2020-07-28 at 11.33.28" style="zoom:50%;" />

### 3 `Conflict feed` : flux de conflits

Pour choisir ce mode il faut cocher `Merge procedure` et ne rien renseigner dans `Stored procedure` :

<img src="assets/Screenshot2020-07-28at11.35.22.png" alt="Screenshot 2020-07-28 at 11.35.22" style="zoom:50%;" />

`Conflict feed` résout rapidement et arbitrairement le conflits.

On peut ensuite programmer la gestion du flux des items refusés et les remettre en place ou les supprimer.

## Les prix de la réplication

<img src="assets/Screenshot2020-07-28at11.39.26.png" alt="Screenshot 2020-07-28 at 11.39.26" style="zoom:50%;" />

La réplication n'ajoute pas de charge, par contre chaque région doit être approvisionnées séparément.

Ce qui signifie pour deux régions minimum `2 X 400 RUs`.

