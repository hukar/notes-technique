# 09 `GZIP`

`IO` dans `dotnet core` est une affaire de `streams`.

```csharp
static void CreateCompressedFile(string inputFilePath, string outputFilePath)
{
    // on ajoute l'extension .gz
    outputFilePath += ".gz";
    Console.WriteLine($"creating {outputFilePath}");

    // On crée un stream en lecture
    var inputFileStream = File.Open(inputFilePath, FileMode.Open);
    // on crée un stream en écriture
    var outputFileStream = File.Create(outputFilePath);
    // On crée un stream de compression avec le stream d'écriture
    var gzipStream = new GZipStream(outputFileStream, CompressionLevel.Optimal);

    // enfin on copie le contenu du stream en lecture dans le stream en compression
    inputFileStream.CopyTo(gzipStream);
}
```

stream lecture : `File.Open(filePath, FileMode.Open)`

stream écriture : `File.Create(filePath)`

stream en compression : `new GZipStream(streamEcriture, CompressionLevel.Optimal)`

copie : `streamLecture.CopyTo(streamCompression)`