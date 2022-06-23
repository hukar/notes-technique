# 00 Changer la version de dotnet sdk

## Pour lister les versions installées  

## `dotnet --list-sdks` :

```bash
kar : udemy-dotnet-core-react $ dotnet --list-sdks

2.1.505 [/usr/local/share/dotnet/sdk]
2.1.701 [/usr/local/share/dotnet/sdk]
2.2.401 [/usr/local/share/dotnet/sdk]
2.2.402 [/usr/local/share/dotnet/sdk]
3.0.100 [/usr/local/share/dotnet/sdk]
```



## Pour voire quelle version est active 

## `dotnet --version` :

```bash
kar : udemy-dotnet-core-react $ dotnet --version
3.0.100
```



## Pour changer de version dans un repertoire

On créé un fichier `global.json` à la racine du répertoire contenant vos projets

### `dotnet new globaljson`

```bash
kar : udemy-dotnet-core-react $ dotnet new globaljson
The template "global.json file" was created successfully.
```

Puis on édite le fichier :

`global.json`

```json
{
  "sdk": {
    "version": "3.0.100"
  }
}
```

```json
// on va le transformer en ça :

{
  "sdk": {
    "version": "2.2.402"
  }
}
```

Maintenant dans la console :

```bash
kar : udemy-dotnet-core-react $ dotnet --version
2.2.402
```

La version de dotnet est bien modifié !