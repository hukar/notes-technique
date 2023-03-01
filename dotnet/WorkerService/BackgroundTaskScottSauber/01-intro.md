# 01 Background Task in `.net 6`

https://www.youtube.com/watch?v=A8i1uQhptTk&ab_channel=JetBrains



## Les problèmes que cela résout

### Les tâches planifiées/`cron jobs`

- Traiter les messages d'une `queue` toutes les X minutes
- Nettoyer une `DB` ou le `File System` toutes les X minutes
- Envoyer une notification par mail toutes les X minutes sous certaines circonstances
- Rafraîchir le `cache` toutes les X minutes
- Vérifier les mises à jour de la `DB` toutes les X minutes



### Realiser des tâches intensives pour le `CPU` de manière asynchrone

- Générer des `PDF`
- Envoyer un email à tous ses clients



## Les options

- `IHostedService`
- `BackgroundService`
- `WorkerService`
- `Hangfire`
- [`Cloud`]



