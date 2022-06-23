# `.net core`

Au début il y a `.net framework` cadre de développement pour Windows.

`Mono` est un projet open source pour transposer `.net framework` sur Linux et Mac.

`Mono` est utilisé dans `Unity` et `Xamarin`.

`.net core` est un recodage de `.net framework` multi plateforme.

`.net standard` est une spécification (une interface) commune que les différentes implémentation doivent suivre.

## `.net core`

<img src="assets/Screenshot2020-10-17at08.58.30.png" alt="Screenshot 2020-10-17 at 08.58.30" style="zoom:50%;" />

Le compilateur s'appelle `Roslyn`.

<img src="assets/Screenshot2020-10-17at09.04.05.png" alt="Screenshot 2020-10-17 at 09.04.05" style="zoom:50%;" />

<img src="assets/Screenshot2020-10-17at09.04.51.png" alt="Screenshot 2020-10-17 at 09.04.51" style="zoom:50%;" />

<img src="assets/Screenshot2020-10-17at09.05.16.png" alt="Screenshot 2020-10-17 at 09.05.16" style="zoom:50%;" />

## `DLL`

## `dotnet restore`

Cette commande crée les dépendances :

<img src="assets/Screenshot2020-10-17at12.48.51.png" alt="Screenshot 2020-10-17 at 12.48.51" style="zoom:50%;" />

Si je supprime ces dossiers, un `dotnet run Program.cs` va aussi exécuter un `restore`:

<img src="assets/Screenshot2020-10-17at12.50.44.png" alt="Screenshot 2020-10-17 at 12.50.44" style="zoom:33%;" />

Le dossier `obj` gère le `cache`.
