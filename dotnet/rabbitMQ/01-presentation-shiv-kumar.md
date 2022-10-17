# 01 Présentation de `RabbitMQ` par Shiv Kumat

https://www.youtube.com/watch?v=sPjy97LcCO0&t=1520s&ab_channel=ShivKumar



## Message Brokers

"Courtiers en messages"

<img src="assets/message-brokers-schema-base.png" alt="message-brokers-schema-base" style="zoom:50%;" />

Le `subscriber` n'est pas forcement `online` lorsque le `publisher` envoie un message dans la `Queue`.

C'est une action `asynchrone`.

Cela permet de découpler complètement le `publisher` du `subscriber`.



## Competing subscribers

Il y a une seule `Queue` avec plusieurs `subscriber`.

Les `subscribers` peuvent être plusieurs instances du même programme.

Ce pattern permet de vider la `Queue` rapidement.

<img src="assets/competing-subscriber-pattern.png" alt="competing-subscriber-pattern" style="zoom:50%;" />

Deux `subscribers` ne peuvent pas avoir le même message, si un `subscriber` prend un message, un autre `subscriber` aura le suivant.



## `Queues` et `Topics`

<img src="assets/topic-exchange-pattern.png" alt="topic-exchange-pattern" style="zoom:50%;" />

Chaque `subscriber` a sa propre `queue`, ce système est proche du fonctionnement un `email` où chaque `queue` est en quelque sorte une boite mail.

Contrairement au `pattern competing subscribers` où chaque message n'est vue que par un et un seul `subscriber`, ici chaque `subscriber` abonné à une queue voit tous ses messages indépendament de ce que font les autres.



## Exemple de l'encodage des fichiers sur `Youtube`

<img src="assets/youtube-message-brokers.png" alt="youtube-message-brokers" style="zoom:50%;" />

`4` est un `subscriber` qui est abonné au `FileReceivedTopic`, il va charger le fichier sur un `storage` externe et envoyer  (publié : `publisher`) un message au `Transcode Topic`.

Ce `subscriber` fonctionne sur la machine cliente.

Dans ce schéma on a un mélange de `Competing Subscribers` (les `transcoder`) et de `Topic Exchange`.



## `Mainframe` scénario

<img src="assets/mainframe-scenario.png" alt="mainframe-scenario" style="zoom:50%;" />

Le `mainframe` envoie des groupes de données importants en taille. L'encryption, la compression et l'envoie peut prendre beaucoup de temps au service `SOAP` utilisant ses propres `Thread`.

On peut décharger le service `SOAP` en utilisant une `Queue` :

<img src="assets/improving-architecture-mainframe-scenarion.png" alt="improving-architecture-mainframe-scenarion" style="zoom:50%;" />

Le service `SOAP` dépose les données sur le `HDD` et envoie un message à `MSMQ` ensuite un `Windows Service` encrypte, compresse et envoie vers `Azure` les données déposées sur le `HDD`. On fait les choses à la fois `asynchronously` et `out-of-band`.

En réalité sur la partie `Azure` on a une plus grande complexité, il y a aussi un `Dashboard` permettant de suivre les différent traitement sur les données.

<img src="assets/complex-scenario-withazure-pass.png" alt="complex-scenario-withazure-pass" style="zoom:50%;" />

Chaque `Worker` va chercher les données sur `Blob Storage`, les traite et les remet sur `Blob Storage` pour le prochain `Worker` en déposant au passage un nouveau message sur `ServiceBus`.



## Scénario des `Logs` (`Load Leveling`)

<img src="assets/load-leveling-pattern-for-logs.png" alt="load-leveling-pattern-for-logs" style="zoom:50%;" />

Plusieurs applications publient leur `logs` à un système centralisé d'analyse de `Logs`.

<img src="assets/load-leveling-with-queues.png" alt="load-leveling-with-queues" style="zoom:50%;" />

`MSMQ` a une limite du nombre de message. Si `RabbitMQ` est `HS` un certain nombre de temps, cela peut poser un problème.

Un `Windows Service ` va copier le `log` sur un `HDD` et désengorger la `MSMQ`. En cas de panne de `RabbitMQ`, le `Windows Service` pourra renvoyer les `logs` enregistrés sur le `HDD`.

Une `Queue` comme `RabbitMQ` supporte une charge de message énorme, pas la `DB`.

Utiliser `RabbitMQ` permet d'aplanir la charge sur la `DB` : `LOAD LEVELING`.

On ralentit ainsi l'arrivée des messages sur la `DB`, on crée un entonnoir avec un seul `Event Subscriber`.

On peut ajouter des `Event Subscriber` pour augmenter le débit ou en supprimer. On gère le débit vers la `DB`.

L'utilisation d'une `Queue` avec un seul `Subscriber` conserve la séquence des événements et l'ordre peut être respectée lors de l'enregistrement en `DB`.



