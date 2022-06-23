# 08 `GUID`

## Global Unique IDentifier

Ce n'est pas vraiment un identifiant unique à 100% mais les risques de collisions sont très faibles.

```csharp
using System;

// - générer un identifiant unique
var uniqueId = Guid.NewGuid();
metadata.File.FileName = uniqueId + ".WAV";
var newPath = Path.Combine("/Users/kar/Desktop/ready_for_transcription", metadata.File.FileName);
```

### `Guid.NewGuid()`

