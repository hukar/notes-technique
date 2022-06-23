# 01 Node.js le cœur

## Le moteur javascript **V8**

`V8` est écris en `C++`.

Il est **open source**, et peut être appelé depuis un programme `C++`.

 ![Screenshot 2020-02-16 at 06.13.31](assets/Screenshot 2020-02-16 at 06.13.31.png)

Javascript est tout en haut de la pile d'abstraction.

**Node.js** ajoute des super pouvoirs à **Javascript**.

**V8** permet de *binder* de nouveaux mots pour son moteur à de nouvelles fonctionnalités développées en `C++`.

> **Node.js** intègre **V8** et donne à **Javascript** des fonctionnalités de langage serveur via `C++`. 

## Node.js Core

## Fonctionnalités utiles pour un serveur

- communiquer sur le réseau
- accéder au système de fichier
- gérer l'organisation du code (require)
- manipuler le protocole `HTTP`
- etc.

#### Code Composition

55 % `javascript` : 27 % `C++` : 18% `python` `C` `other`

**Node.js** se compose de deux cœurs :

#### `C++` core | `Javascript` core

