# 04 `OAuth` pour les `applications` côté serveur (`server side`)



 ## Enregister son `app`

On doit enregistrer son `app` auprès d'un serveur `OAuth`, on doit lui fournir une `URL` de redirection.

On donne quelque information sur son `app` et le `OAuth server` nous retourne un `Client ID` et si c'est un `Confidential Client` un `Client sercret`.

Ces données sont nécessaire pour les afficher aux `utilisateurs` ou pour renseigner les `logs`.

On doit au moins enregistrer le `name` de l'`app` et une ou plusieurs `URL` de redirection.

Cet (ces) `URL(s)` de redirection empêche un attaquant d'utiliser votre `Client ID`, car il ne peut venir de la même `URL`.

Cette `URL` ne doit pas contenir de `wildcard` pour éviter des attaques.

l'`URL` est vraiment le seul moyen pour l'`authorization server` de savoir que ce n'est pas un attaquant qui essaye d'usurper l'identité de l'`app` réel.

L'`app` peut passer son `Client ID` de façon visible au `OAuth server`.

Une fois l'`app` enregistrer, elle est prête à commencer un `OAuth Flow`.

Le `Client ID` est considéré comme donnée public, il peut être visible dans le code source.

L'`Authorization server` ne doit pas renvoyer un `Client Secret` (`password`) pour une `application mobile` ou une `SPA`, si même il le renvoyait, il ne faudrait pas l'utiliser (car il n'y a aucun moyen de le rendre véritablement secret).

Dans le cas d'une `application serveur`, le `client secret` peut être gardé dans le code de l'`application` car ce code n'est pas lu (accessible) par l'`utilisateur`.

