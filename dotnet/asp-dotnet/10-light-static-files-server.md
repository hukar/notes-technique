# Un serveur de fichier statique très léger

On part d'une application web vide :

```bash
dotnet new web -o LiteServer
```

On ajoute un dossier `wwwroot` (le dossier par défaut).

On modifie `Startup.js` :

```cs
public class Startup
{
    public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
    {
        app.UseFileServer();
        app.UseDirectoryBrowser();
    }
}
```

C'est `UseDirectoryBrowser` qui permet de naviguer dans les dossiers du serveur.