# 03 l'extension d'un `fichier`



## déterminer l'`extension` d'un `fichier` : `Path.GetExtension`

On voudrait pouvoir appliquer un `Process` particulier sur les fichiers `.txt`

```cs
	// Determine type of file
	string extension = Path.GetExtension(InputPathFile);

	switch(extension)
    {
        case ".txt":
            ProcessTextFile(inProgressFilePath);
            break;
        default:
            WriteLine($"{extension} is not supported");
            break;
    }
}

private void ProcessTextFile(string inProgressFilePath)
{
    WriteLine($"Processing text file {inProgressFilePath}");
}
```

`Path.GetExtension` récupère l'extension avec le point devant : `.txt`, `.png`, ...



## Modifier l'`extension` d'un fichier

```cs
// Moving file after processing is complete
string completedDirectoryPath 
    = Path.combine(rootDirectoryPath, CompletedDirectoryName);
Directory.CreateDirectory(completedDirectoryPath);

string fileNameWithCompletedExtension = 
    Path.ChangeExtension(inputFileName, ".complete");
string completedFileName = 
    $"{Guid.NewGuid()}_{fileNameWithCompletedExtension}";

string completedFilePath = Path.Combine(completedDirectoryPath, completedFileName);

WriteLine($"Moving {inProgressFilePath} to {completedFilePath}");
File.Move(inProgressFilePath, completedFilePath);
```

<img src="assets/result-of-file-processing-directories-and-files.png" alt="result-of-file-processing-directories-and-files" style="zoom:50%;" />

















