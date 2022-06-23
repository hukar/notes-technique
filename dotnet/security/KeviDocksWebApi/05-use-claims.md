# 05 Récupérer le `User` dans le `endpoint`



## ClaimsPrincipal `User`

Dans le contrôleur le `User` est disponible sous le type `ClaimsPrincipal` où l'identité est représentée par une liste de `Claims`.

Dans une `Action`  du `Controller` :

```cs
[HttpGet]
public IActionResult GetRobots()
{
    var lastName = User.FindFirst(ClaimTypes.Surname);

    Console.WriteLine(lastName);
```

On obtient ceci :

```
Last Name : http://schemas.xmlsoap.org/ws/2005/05/identity/claims/surname: Kar
```

> ??? un meilleur moyen ???

Voici des exemples pour les autres `Claims` :

```cs
var nameIdentifier = User.FindFirst(ClaimTypes.NameIdentifier);
var name = User.FindFirst(ClaimTypes.Name);
var givenName = User.FindFirst(ClaimTypes.GivenName);
var surname = User.FindFirst(ClaimTypes.Surname);
var email = User.FindFirst(ClaimTypes.Email);
var mobilePhone = User.FindFirst(ClaimTypes.MobilePhone);
var authenticationMethod = User.FindFirst(ClaimTypes.AuthenticationMethod);
var emails = User.FindFirst("emails");
```

