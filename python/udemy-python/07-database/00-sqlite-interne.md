# 00 SQLite interne

## rowid

Les types sont dynamiques dans sqlite, et une colonne peut stocker tous les types.

Sauf pour `integer primary key` qui est en fait un alias de la colonne interne `rowid` (= adresse unique à la table d'enregistrement).

`rowid` est en entier de 64 bits.

**transaction** action modifiant la base de données

```sh
BDD state A --transaction--> BDD state B
```

à peu près tout sauf `SELECT`

Transaction atomique (= indivisible) le programme se déroulera jusqu'à la fin sans céder le monopole sur ses données.