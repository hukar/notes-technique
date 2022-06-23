# 02 Implémentation de la sécurité `Token-based`

## Login

On doit implémenter un `endpoint` gérant le `login`.

```
POST api/login
```



## Anatomie du `token`

Un `token` se compose de troie parties :

### 1 Le `payload`

<img src="assets/token-payload-first-party.png" alt="token-payload-first-party" style="zoom:33%;" />

Ce sont des infos sur l'utilisateur au départ en `json`



### 2 La signature

C'est un `hash` du payload, de façon que si le `payload` est modifié après la génération du `token`, la signature ne correspond plus et on sait que le `token ` a été altéré.

<img src="assets/token-signature-hash-payloed-second-part.png" alt="token-signature-hash-payloed-second-part" style="zoom:33%;" />

Pour signer on a besoin d'une clé (`key`) générée depuis un `secret`.



### 3 Le `header`

<img src="assets/the-token-header-third-part.png" alt="the-token-header-third-part" style="zoom:33%;" />

Ce sont des informations sur le type de `token` et l'algorithme utilisé.



## Implémentation

<img src="assets/steps-for-implementing-token-security.png" alt="steps-for-implementing-token-security" style="zoom:33%;" />













