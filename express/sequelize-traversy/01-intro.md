# 01 Sequelize Intro (avec MySQL)

## Installation

```bash
npm i mysql2 sequelize
```

Installation des outils en ligne de commande :

```bash
npm i sequelize-cli
```

## Initialisation du projet : `database.js`

```js
const { Sequelize } = require('sequelize')

const sequelize = new Sequelize('sequelize_db', 'root', '', {
    dialect: 'mysql',
    host: 'localhost',
})

module.exports = sequelize
```





## Création du modèle `models/product.js`

