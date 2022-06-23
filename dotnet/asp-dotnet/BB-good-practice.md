# BB Les bonnes pratiques

## Les `class` sont par défaut `internal` et `sealed`

Dans une application de gestion, on ne oartage pas ses `class` par défaut.

Ce sont les librairie `nuget` qui partage leurs classes avec l'application pas l'inverse.

`internal` accessible que dans la même `dll`  (`assembly`)

`sealed` aucune classe ne peut hériter d'une `sealed class`

Les classes par défaut devrait toutes être :

```cs
internal sealed class MyClass { ... }
```



## Ne pas inventer de nom

S'il n'y a pas de risque de collision, la variable prend simplement le nom de sont type en `camelCase` :

```cs
HttpClient httpClient;
CancellationTokenSource cancellationTokenSource;
```

Si les classes sont bien nommées, alors les variables et les paramètres doivent prendre le nom du type.

De même si une méthode `GetSomething` retourne sa valeur à une variable, celle-ci doit s'appeler tout simplement `something`.

```cs
// PAS BON
var token = GetAccessToken();

// BON
var accessToken = GetAccessToken();
```





## Ne pas passer plus de données que nécessaire à une `Méthode`

Si une méthode à besoin de deux valeurs pour sa logique, il ne faut pas lui passer un objet contenant 11 valeurs dont les deux voulus.

À la place il faut créer une classe répondant au besoin de la `méthode` et mapper les valeurs dans cette classe.

On peut utiliser les `records` pour ça.



## Méthode d'orchestration

La méthode d'orchestration est la seule méthode `public`.

Elle appelle d'autre méthodes `private` pour raconter l'histoire de ce qu'elle fait.

```cs
public async Task HandleRecordNewOrder()
{
  await LoginToTheServer(_utilities.Credentials);
  var response = await SendNewOrderToTheServer(newOrderToRecord);
  await LogOutToTheServer();
}
```

Les implémentation technique doivent rester dans des méthodes privées et bien nommées.



## Méthode `private static`

Déclarer ses méthodes `private` comme `static` si elle n'utilise aucune valeur de l'état de la classe.

Une méthode `private static` ne modifie pas le `state` de l'objet.

En ajoutant `static` on communique sur les effets de la méthode.

> Un appel à une méthode `static` génère une instruction d'appel en langage intermédiaire Microsoft (MSIL), tandis qu'un appel à une méthode d'instance génère une instruction `callvirt`, qui vérifie également les références d'un objet `null`. Cependant, la plupart du temps, la différence de performance entre les deux n'est pas significative.