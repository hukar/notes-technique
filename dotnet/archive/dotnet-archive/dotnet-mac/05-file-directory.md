# 05 Fichier et répertoire

```csharp
using System;
using System.IO;

namespace dictation_processor
{
    class Program
    {
        static void Main(string[] args)
        {

            var subfolders = Directory.GetDirectories("/Users/kar/Desktop/uploads");
            // iterer à travers le sous-dossier uploads
            foreach(var subfolder in subfolders)
            {
                // obtenir le fichier des meta-données
                var metadataFilePath = Path.Combine(subfolder,"metadata.json");
                System.Console.WriteLine(metadataFilePath);
                // extraire les meta-données
                var metadataFileStream = File.Open(metadataFilePath, FileMode.Open);
                // ...
            }
        }
    }
}
```

## `Directory.GetDirectories(Path)`

Retourne un tableau de chaine de caractère avec le chemin vers les répertoires internes :

<img src="assets/Screenshot2020-10-18at15.05.11.png" alt="Screenshot 2020-10-18 at 15.05.11" style="zoom:50%;" />

## `Path.combine(params string[])`

Prends une liste d'arguments pour construire un chemin.

## `File.Open(path, Mode)`

Ouvre un fichier dans un mode particulier sous la forme d'un stream.

<img src="assets/Screenshot2020-10-18at15.08.50.png" alt="Screenshot 2020-10-18 at 15.08.50" style="zoom:50%;" />
