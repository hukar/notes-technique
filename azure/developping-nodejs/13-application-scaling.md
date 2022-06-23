# 13 Application Scaling

<img src="assets/Screenshot2020-07-16at16.05.19.png" alt="Screenshot 2020-07-16 at 16.05.19" style="zoom:50%;" />

`Scale Up` revient à prendre une machine plus performante :

<img src="assets/Screenshot2020-07-16at16.06.03.png" alt="Screenshot 2020-07-16 at 16.06.03" style="zoom:50%;" />

On revient au choix de sa configuration.

`Scale Out` permet de modifier le nombre d'instance de sa machine :

<img src="assets/Screenshot2020-07-16at16.07.14.png" alt="Screenshot 2020-07-16 at 16.07.14" style="zoom:50%;" />

`Manual Scale` permet de changer ce nombre manuellement.

`Custom Autoscale` permet de mettre des conditions à la mise à l'échelle (scaling).

On règle le nombre d'instance minimum et maximum :

<img src="assets/Screenshot2020-07-16at16.10.13.png" alt="Screenshot 2020-07-16 at 16.10.13" style="zoom:50%;" />

## Ajout de règles

### Ajouter des instances

<img src="assets/Screenshot2020-07-16at16.12.12.png" alt="Screenshot 2020-07-16 at 16.12.12" style="zoom:50%;" />

Ici si la charge `cpu` est supérieur à 70% pendant 10mn on ajoute une instance.

`Cool down` signifie qu'après une nouvelle instance le système atant 5mn que la charge se stabilise avant d'jouter encore une nouvelle instance.

### Retirer des instances

Il faut maintenant ajouter une règle pour redescendre le nombre d'instances.

<img src="assets/Screenshot2020-07-16at16.17.06.png" alt="Screenshot 2020-07-16 at 16.17.06" style="zoom:50%;" />

On a donc deux règles :

<img src="assets/Screenshot2020-07-16at16.19.08.png" alt="Screenshot 2020-07-16 at 16.19.08" style="zoom:50%;" />

### Augmenter le nombre d'instances pour une date spécifique

Si on sait qu'à une période donnée on va avoir besoin de plus de ressource on peut le programmer.

<img src="assets/Screenshot2020-07-16at16.23.51.png" alt="Screenshot 2020-07-16 at 16.23.51" style="zoom:50%;" />

On peut aussi se baser sur les jours ouvrable de la semaine :

<img src="assets/Screenshot2020-07-16at16.25.14.png" alt="Screenshot 2020-07-16 at 16.25.14" style="zoom:50%;" />
