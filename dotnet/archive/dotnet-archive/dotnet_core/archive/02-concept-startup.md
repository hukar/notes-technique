# 02 concepts startup.cs

##  Program.cs

Deux serveur HTTP interne à .net core :

1. Kestrel : multi-plateforme
2. WebListener : Windows

Kestrel est assez bon pour être utilisé en intranet mais pur le web, il faut utiliser IIS ou Apache.



## Injection de dépendance startup.cs

Les services utilisent l'injection de dépendance :

### dans `startup.cs` : `ConfigureServices`

```c#
   public void ConfigureServices(IServiceCollection services /* un conteneur de service en argument */)
        {
           // on déclare ici les services à injecter
       services.AddMvc().SetCompatibilityVersion(CompatibilityVersion.Version_2_2);

            // In production, the Angular files will be served from this directory
            services.AddSpaStaticFiles(configuration =>
            {
                configuration.RootPath = "ClientApp/dist";
            });
        }

```

Voilà à quoi ressemble l'ajout d'une injection de dépendence :

```bash
 services.AddTransient<IRepository, Respository>();
```

Dès que Repository apparaît dans un constructeur, la classe Repository est injectée.

### La méthode configure

Quand la requête arrive dans le serveur Web, elle entre dans un ***Pipeline***.

Dans ce *Pipeline* il y a plusieurs composants apellé **Middleware**.

Dans le fichier `Configure.cs` on va pouvoir créer ce *Pipeline* pour l'environnement de **dev** ou de **prod**

```c#
// Middleware autorisant les fichiers statique : obligatoire
app.UseSpaStaticFiles();
// Middleware gérant les routes
app.UseMvc(routes =>
           {
               routes.MapRoute(
                   name: "default",
                   template: "{controller}/{action=Index}/{id?}");
           });
```

