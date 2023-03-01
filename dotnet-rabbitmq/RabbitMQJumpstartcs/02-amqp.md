# 02 `AMQP`

## Advanced Message Queuing Protocole

Tout comme en programmation objet, `AMQP` utilise des commandes définies par une `class` et une `method`.

Par exemple `exchange` est la `class` et `declare` est la `methode`, l'ensemble forme une `command`.

Lorsqu'une `command` est envoyée à un `broker` (`rabbitmq`), elle est contenu dans une structure de données standardisée nommée `frame`.



## `Frames`

Il y a quatre types de `frame`:

- `Method Frame`
- `Content Header Frame`
- `Body Frame`
- `Heartbeat Frame`

<img src="assets/amqp-frame-structure.png" alt="amqp-frame-structure" />

<img src="assets/specific-content-of-frame.png" alt="specific-content-of-frame" />

`40` est l'`ID` qui précise la `class` ici `exchange`.

`10` est l'`ID` qui précise la `method` ici `declare`.

Chaque communication commence par une `Method Frame`.



## Communication `Client` `Broker`

### `Connection` negociation

