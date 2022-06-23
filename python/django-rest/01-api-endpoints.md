# 01 API, JSON et ENDPOINTS

## Qu'est-ce qu'une API

### API = Application Programming Interface

L'ensemble des méthodes procurant une interface de programmation au développeur.

L'API permet de ne pas se préoccuper de ce qu'il y a derrière le nom de ses méthodes.

Ce sont des briques de construction software.

## Web API

La plus commune étant `REST`

Utilisation du `json`

### JSON = JavaScript Object Notation

C'est une écriture littéral de `set` composé par des couples de clé / valeurs

```js
> a = {"titi":56,"tata":32,"titi":79}
{ titi: 79, tata: 32 }
```

On voit ici que les objets littéraux se comportent comme des `set`, pas de doublons dans les clés.

`set` : pas d'ordre particulier, pas de répétition = ensemble fini

## ENDPOINTS

C'est la fin du canal de communication où un service peut être joint.

Le `ENDPOINTS` est représenté par une `URL`

```
/api/companies/5/employees/3
/api/v2/companies/5/employees/3
/api/employees/3
```

