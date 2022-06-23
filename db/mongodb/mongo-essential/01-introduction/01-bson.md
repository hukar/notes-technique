# 01 Introduction `mongoDB`

## `JSON`

Il y a 6 types en `json`

1. string
2. number
3. boolean
4. array
5. object
6. null

## `BSON`

`B`inary `json`.

C'est le format qu'utilise `mongoDB` pour enregistrer les données.

`bson` supporte plus de type de données.

|                                                                                                                      |                                                                                                                     |
| -------------------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------- |
| - string<br /> - double<br /> - 32 bits integer<br /> - 64 bits integer<br /> - boolean<br /> - array <br />- Object | - null<br /> - regular expression<br />- timestamp<br />- date<br />- objectId<br />- binary data<br />- others ... |

## Convertir `json` en `bson` et vice versa : `extended json`

<img src="assets/Screenshot2020-03-24at09.33.44.png" alt="Screenshot 2020-03-24 at 09.33.44" style="zoom:50%;" />

Il existe deux modes `shell mode` et `strict mode`.

### `shell mode`

- Types `bson` en ligne
- le mode interne de `mongoDB`
- Le shell de `mongoDB` comprends ce mode

### `strict mode`

- Représente les types `bson` utilisant un nom de clé spécial commençant par `$`
- standard `json` compatible
- Fonctionne avec les parser `json`

Les deux modes sont supportés par les driver externe de `mongoDB` et les `api rest`.

Supporté aussi par `mongoimport utility`.

<img src="assets/Screenshot2020-03-24at09.44.46.png" alt="Screenshot 2020-03-24 at 09.44.46" style="zoom:50%;" />

On voit que le `strict mode` est du `json` valide.
