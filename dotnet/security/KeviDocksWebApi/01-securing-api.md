# 01 Sécurisé son API

Une `API` doit être public pour pouvoir être accédée par une application `front` ou une autre `API`.

Envoyer à chaque requête `username/password` est une mauvaise idée

-  c'est lent
- C'est une importante cause d'attaque



Une meilleur idée est d'utiliser les `Token`



## `Token-based` Security

- On envoie un `token` à chaque requête
- Un `token` représente une autorisation (`consent`)
- Le `token` est validé au niveau de l'`API`

Cette approche fonctionne pour toutes les architecture moderne.

`OAuth` et `OpenID Connect` sont des protocoles basés sur l'utilisation des `tokens`.