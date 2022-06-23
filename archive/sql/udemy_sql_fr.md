# Cheat Sheet SQL

```sql
--LECTURE
SELECT DISTINCT name FROM cinema --select les enr. distincts

--ECRITURE
--on peut insérer plusieurs valeurs en même temps
INSERT INTO language (name) VALUES ('Persian'),('Vietnamese');

--MODIFICATION
--Tous les enregistrement sont modifiés s'il n'y a pas de clause WHERE
--On peut modifier plusieurs colonne en même temps
UPDATE `film` 
SET `language_id`=1,`original_language_id`=2
UPDATE table 
SET colonne_1 = valeur_1, colonne_2 = valeur_2

--SUPRESSION
--Supprime rous les enregistrements de la table
DELETE FROM nom_table

--CONDITION
--clause WHERE 
--------------------
--trois opérateurs logique AND, OR, NOT
SELECT cinema_id
FROM cinema
WHERE NOT ((cinema_id > 8 OR cinema_id < 6) AND cinema_id != 15)
-- on obtient les ids 6, 7, 8, 15
SELECT *
FROM film
WHERE NOT budget = 0 -- => budget != 0 même chose
--IN, BETWEEN et IS NULL
--1. IN
--recherche dans une liste de possibilité (a, b, c, ...)
SELECT * FROM 	room WHERE name NOT IN ('A','B') AND number_places IN (70, 65)
--2. BETWEEN
-- col BETWEEN v1 AND v2 inclus les valeurs v1 et v2
--=> col >= v1 AND col <= v2
SELECT * FROM 	room WHERE  number_places  BETWEEN 60 AND 70

--ALIAS
--Créer un alias avec la clause AS
SELECT name AS nom, city AS ville FROM cinema
--On peut aussi écrire sans le AS
SELECT name nom, city ville FROM cinema
--Mettre le résultat d'un calcul dans une nouvelle colonne
SELECT val1 + val2 AS result_add FROM numbers
--l'alias sert aussi à renommer des tables
SELECT * FROM members AS m

--TRIER
--clause ORDER BY
SELECT * FROM table [WHERE cond] ORDER BY column ASC / DESC
--On peut donner une deuxième colonne pour le trie si la première a deux valeurs identiques
SELECT * FROM table ORDER BY col1, col2
SELECT * FROM room	 WHERE 1 ORDER BY name, number_places, cinema_id  --ici trois colonnes
--On peut mixer avec des ASC (plus petit au plus grand) et des DESC (plus grand au plus petit)
SELECT * FROM room	 WHERE 1 ORDER BY name DESC, number_places ASC, cinema_id DESC

--LIMITER
--Pour limiter le nombre de ligne on utilise la clause LIMIT
SELECT * FROM room	 WHERE 1 ORDER BY name DESC LIMIT 10
--ceci est équivalent à
SELECT * FROM room	 WHERE 1 ORDER BY name DESC LIMIT 0, 10
--On peut aussi spécifier en premier argument l'index de débute (commence à 0)
SELECT * FROM room	 WHERE 1 ORDER BY name DESC LIMIT 6, 10
--affiche les lignes de l'index 6 à l'index 15

--FONCTIONS
--Les fonctions d'agrégation permettent de faire des statistiques
--COUNT() -> compte les lignes
--AVG() -> average fait la moyenne
--SUM() -> fait la somme
--MIN() -> renvoie le plus petit
--MAX() -> renvoie le plus grand
SELECT COUNT(*) as total FROM film -- 10
SELECT AVG(budget) as moy_budget FROM film 
SELECT SUM(number_places) AS total_places FROM room
SELECT MIN(number_places) AS min_places FROM room -- 40
SELECT MAX(number_places) AS max_places FROM room -- 100
--utilisation de COUNT et de DISTINCT en même temps
SELECT COUNT(DISTINCT number_places) AS nb_diff_places FROM room -- 9
--combien il y a de nombres_de_places distinct = nombre de valeurs différentes dans une colonne
--exemples:
SELECT *, MAX(number_rooms) AS rooms FROM cinema
SELECT AVG(budget) as MOY_BUDGET, AVG(running_time/60) AS MOY_HOURS FROM film
```

