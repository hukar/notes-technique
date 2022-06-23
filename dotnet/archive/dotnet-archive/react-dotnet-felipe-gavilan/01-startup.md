# 01 Démarrage

Mettre à jour `node` et `.net`.

Mettre à jour `EF Core CLI Tool`

```bash
dotnet tool update --global dotnet-ef
```



## Créer l'application `React`

```bash
npx create-react-app react-movies --template typescript
```



## Faire tourner l'application

```bash
npm start
```



## `Dotnet API`

```bash
dotnet new wepapi -o MoviesAPI
```

```bash
dotnet watch run
```

Si on a un problème avec `https` on doit lancer cette commande :

```bash
dotnet dev-certs https --trust
```

