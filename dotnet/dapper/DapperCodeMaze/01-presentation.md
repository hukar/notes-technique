# 01 Présentation de `Dapper`

D'après la vidéo :

https://www.youtube.com/watch?v=C763K-VGkfc&t=233s&ab_channel=CodeMaze



## Les méthodes d'`extension`

`Dapper` propose d'ajouter des méthodes d'extension autour de `DbConnection`.

`Execute` retourne le nombre de ligne affectée.

`Query` on peut exécuter une requête et *mapper* le résultat.

`QueryFirst` *mappe* le premier résultat.

`QueryFirstOrDefault` *mappe* le premier résultat ou renvoie la valeur par défaut si aucun élément n'est retourné.

`QuerySingle` *mappe* le résultat. Lance une `exception` si la réponse ne contient pas exactement un élément.

`QuerySingleOrDefault` la même chose que `QuerySingle` mais renvoie la valeur par défaut si la réponse est vide.

`QueryMultiple` exécute plusieurs requête avec une seule commande et *mappe* le résultat.

Il existe pour chaque méthode une version `async`.



## Installation

```bash
dotnet add package Microsoft.Data.SqlClient
dotnet add package dapper
```




