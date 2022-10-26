# 01 gérer les `fichiers` et les `dossiers`



## Vérifier qu'un `fichier` existe : `File.Exists`

On créé un programme `console` :

> `args[nb]` est le tableau d'arguments de la commande, la commande en elle-même étant à l'indice `0`.
>
> On peut renseigner les arguments du `debugger` de `vscode` dans `.vscode/launch.json` comme ceci :
>
> ```json
> // ...
> "program": "${workspaceFolder}/DataProcessor/bin/Debug/net7.0/DataProcessor.dll",
>             "args": ["--file","/Users/hukar/Documents/programmation/dotnet/FileAndDirectoryJasonRoberts/DataFile/FileText01.txt"],
> // ...
> ```
>
> 

```cs
WriteLine("Parsing command line options");

string command = args[0]; 

if(command == "--file")
{
    string filePath = args[1];
    WriteLine($"Single file {filePath} selected");
    ProcessSingleFile(filePath);
}
else if(command == "--dir")
{
    string directoryPath = args[1];
    string fileType = args[2];
    
    WriteLine($"Durectory {directoryPath} selected for {fileType} files");
    ProcessDirectory(directoryPath, fileType);
}
else
{
    WriteLine("Invalid command line options");
}

WriteLine("Press enter to quit.");
ReadLine();

static void ProcessSingleFile(string filePath)
{
    FileProcessor fileProcessor = new(filePath);
    fileProcessor.Process();
}

static void ProcessDirectory(string directoryPath, string fileType)
{
    
}
```



Et une classe `FileProcessor` :

```cs
public class FileProcessor
{
    public string InputPathFile { get; }

    public FileProcessor(string inputPathFile)
    {
        InputPathFile = inputPathFile;
    }

    public void Process()
    {
        WriteLine($"Begin process of {InputPathFile}");
        
        // check if file exists
        if(File.Exists(InputFilePath) == false)
        {
            WriteLine($"ERROR: file {InputFilePath} does not exist.");
            return;
        }
    }
}
```



### Vérifier que c'est un chemin absolu









## Vérifier qu'un `dossier` existe

