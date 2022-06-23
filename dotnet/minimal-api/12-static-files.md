# 12 Servire des `Fichiers`

## `app.UseStaticFiles`

Par convention `UseStaticFiles` utilise le dossier `wwwroot`

```cs
app.UseStaticFiles();
```

<img src="assets/wwwroot-static-file.png" alt="wwwroot-static-file" style="zoom:50%;" />

On a l'`url` :

```
https://localhost:7136/contact.json
```



### On peut changer l'`url` : `UseStaticFile("newUrl")`

```cs
app.UseStaticFiles("/popo");
```

<img src="assets/new-url-use-static-file.png" alt="new-url-use-static-file" style="zoom:50%;" />



### On peut aussi changer le dossier `wwwroot`

```cs
using Microsoft.Extensions.FileProviders;

app.UseStaticFiles(new StaticFileOptions
{
    FileProvider = new PhysicalFileProvider(
           Path.Combine(builder.Environment.ContentRootPath, "TotoServiceFiles")),
    RequestPath = "/TotoFiles"
});
```

<img src="assets/new-file-provider-static-file.png" alt="new-file-provider-static-file" style="zoom:50%;" />

<img src="assets/all-new-url-file-provider-static-file.png" alt="all-new-url-file-provider-static-file" style="zoom:50%;" />