# 04 `TSQL` avancé



## Les alias : `AS`

```sql
SELECT Nom AS Pseudo FROM Etudiant
```



## Créer la `DB` `Contact`

Problème lors de l'insertion des dates :

<img src="assets/inserting-date-failed-rtc.png" alt="inserting-date-failed-rtc" style="zoom:50%;" />

Le problème ici c'est que c'est le format français et pas le format américain (sinon ça peut fonctionner juste avec de simple guillemet).

> On peut utiliser `sp_helplanguage` pour obtenir des informations sur les formats utilisés
>
> <img src="assets/sp-helplanguage-display-uib.png" alt="sp-helplanguage-display-uib" style="zoom:50%;" />
>
> Pour connaitre son langage :
>
> ```sql
> select @@language;
> ```
>
> <img src="assets/select-language-info-jss.png" alt="select-language-info-jss" style="zoom:50%;" />
>
> Pour le modifier :
>
> ```sql
> SET LANGUAGE Français
> ```
>
> Ou juste le format de la date :
>
> ```sql
> SET DATEFORMAT 'dmy'
> ```
>
> `d` day, `m` month, `y` year on a `mdy`, `dmy` ou encore `ymd`.