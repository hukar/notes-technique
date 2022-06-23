# 03 Les versions de `dotnet`

## Les versions installées

> .NET Core is a runtime. It can execute applications that are built for it.
>
> ASP.NET Core is a collection of libraries that form a Framework for building web applications.

On trouve les dossiers pour les deux :

```bash
🦄 dictation-processor which dotnet
/usr/local/share/dotnet/dotnet

🦄 dictation-processor cd /usr/local/share/dotnet/

🦄 dotnet ls
LICENSE.txt             host                    shared
ThirdPartyNotices.txt   packs                   templates
dotnet                  sdk

🦄 dotnet cd shared
🦄 shared ls
Microsoft.AspNetCore.All        Microsoft.NETCore.App
Microsoft.AspNetCore.App

🦄 shared cd Microsoft.AspNetCore.App # le framework
🦄 Microsoft.AspNetCore.App ls
2.1.12  2.1.9   2.2.6   2.2.7   3.0.0   3.1.5   3.1.6   3.1.8

🦄 Microsoft.AspNetCore.App cd ../Microsoft.NETCore.App/ # le runtime
🦄 Microsoft.NETCore.App ls
2.1.12  2.1.22  2.2.6   3.0.0   3.1.6
2.1.20  2.1.9   2.2.7   3.1.5   3.1.8

🦄 Microsoft.NETCore.App cd ../Microsoft.AspNetCore.All
🦄 Microsoft.AspNetCore.All ls
2.1.12  2.1.9   2.2.6   2.2.7
```

`Microsoft.AspNetCore.All` est un package déprécié, on voit qu'il n'apparait plus pour les versions `3.*`.

#### Il y a donc `Microsoft.NETCore.App` pour le `runtime` et `Microsoft.AspNetCore.App` pour le framework web.

