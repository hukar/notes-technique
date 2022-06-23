# 04 Mini Projet

```csharp
static void Main(string[] args)
{
    // iterer à travers le sous-dossier uploads
    // obtenir le fichier des meta-données 
    // extraire les meta-données

    // pour chaque fichier audio
    // - obtenir le chemin absolu
    // - verifier la checksum
    // - générer un identifiant unique
    // - compresser le fichier
    // - créer un fichier de méta-données par fichier audio
}
```

## Création des classes `Metadata` et `AudioFile`

`Metadata.cs`

```csharp
using System;
using System.Collections.Generic;

public class Metadata
{
    public string Practitioner { get; set; }
    public string Patient { get; set; }
    public DateTime DateRecorded { get; set; }
    public List<string> Tags { get; set; }
    public AudioFile File { get; set; }
}
```

`AudioFile.cs`

```csharp
public class AudioFile
{
    public string fileName { get; set; }
    public string Md5Checksum { get; set; }
}
```

