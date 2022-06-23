# 02 `Services` et `Middlewares`

Un `middleware` est un composant permettant de traiter la requête envoyée au site `ASP.NET`.

Le middleware a deux possibilités :

1. IL NE SAIT PAS RÉPONDRE et il passe la requête au `middleware` suivant.
2. IL SAIT COMMENT RÉPONDRE et court-circuite la chaîne des `middleware` en produisant la réponse.

Un `middleware` sert aussi à enrichir les informations du contexte de la requête (comme l'authentification).

<img src="assets/cookie-authenticated-added-by-middleware-fss.png" alt="cookie-authenticated-added-by-middleware-fss" style="zoom:50%;" />