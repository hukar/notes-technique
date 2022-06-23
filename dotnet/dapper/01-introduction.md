# 01 Introduction

## Qu'est-ce que `Dapper` ?

C'est un micro `ORM` pour `.net`.

Créer par la team de `stack exchange`.

Un `ORM` (`Object` `Relational` `Mapper`) a pour rôle de *mapper* les objets avec les données de la `DB`.



## `ORM` vs `Micro ORM`

`Entity Framework` qui est un `ORM`, va *mapper* les objets avec la `DB` mais va aussi générer le `SQL` et suivre (`tracking`) les changements.

Un `Micro ORM` comme `Dapper` ne va faire que le `mapping` laissant le `SQL` et le `change tracking` à la charge du développeur.



## Fonctionnalités d'un `Micro ORM`

- Léger
- Rapide
- simple d'utilisation

On a un total contrôle sur le `SQL`, pas de configuration compliquée, la courbe d'apprentissage est rapide.



## Les fonctionnalités clé de `Dapper`

- Le mapping dans les deux sens
- Le paramétrage des requêtes : pas d'injection `SQL` possible.
- Des performances *"proche du métal"* (code de bas niveau plus rapide)
- Une `API` simple : trois principales méthodes `Query`, `Query Dynamic` et `Execute`

- N'importe quelle `Database` ayant un provider car `Dapper` hérite de `IDbConnection`

