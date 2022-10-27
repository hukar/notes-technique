# 05 Énumérer les `fichiers` d'un `dossier`



## `Directory.GetFiles`

### Tous les `fichiers`

```cs
// All files
string[] allFiles = Directory.GetFiles(directoryPath);
```



### Les `fichiers` d'un type donné

```cs
static void ProcessDirectory(string directoryPath, string fileType)
{
    switch(fileType)
    {
        case "TEXT":
            string[] textFiles = Directory.GetFiles(directoryPath, "*.txt");
            foreach(var textFilePath in textFiles)
            {
                var fileProcessor = new FileProcessor(textFilePath);
                fileProcessor.Process();
            }
            break;
        default:
            WriteLine($"ERROR: {fileType} is not supported");
            return;
    }
}
```

### `Directory.GetFiles(directoryPath, searchPattern)`

Le `searchPattern` peut utiliser des `wildcard` comme `*` et `?`.