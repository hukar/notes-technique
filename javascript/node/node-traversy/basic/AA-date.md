# Gestion des dates en javascript

## `UTC` Universale Time Coordonated

Temps Universel Coordonné lié au temps atomique universelle avec un écart en seconde.

Dans `MongoDB` le temps est sauvegardé au format `UTC`.

Voici quelques conversions grâce au javascript :

```js
Date.now() //? 1588844775336
new Date(Date.now()) //? Thu May 07 2020 11:46:15 GMT+0200 (Central European Summer Time)

// temps sauvé dans mongoDB 2020-05-06T14:36:59.598+00:00
new Date("2020-05-06T14:36:59.598+00:00") //? Wed May 06 2020 16:36:59 GMT+0200 (Central European Summer Time)
```

`Date.now()` entier : nombre de millisecondes depuis le 1 janvier 1970 00:00:00 UTC