# 03 Scripting

## Créer un `Script`

Clique droit dans la fenêtre `Assets` puis glisser vers un élément :

<img src="assets/create-script-assets.png" alt="create-script-assets" style="zoom:50%;" />

<img src="assets/drag-and-drop-script.png" alt="drag-and-drop-script" style="zoom:50%;" />



## Utilisation de `Callback`

```cs
public class PrintToConsole : MonoBehaviour
{
    void Start()
    {
        Debug.Log("I speak in default Console !!");
    }

    void Update()
    {
        
    }
}
```

Le `Script` se compose de deux `Callback`.

`Start` est exécuté avec le premier rendu.

`Update` est exécuté à chaque rendu (chaque `Frame`).



## `Debug.Log`

Écris dans la `Console` de l'interface de `Unity` :

```cs
public class PrintToConsole : MonoBehaviour
{
    // Start is called before the first frame update
    void Start()
    {
        Debug.Log("I speak in default Console !!");
    }

    // Update is called once per frame
    void Update()
    {
        
    }
}
```

<img src="assets/unity-console-ui.png" alt="unity-console-ui" style="zoom:50%;" />



## `Update()`

S'exécute à chaque `frame`.

Pour connaitre le nombre de `fps` (`frame per second`), on peut afficher les `stats` dans la fenêtre de rendu :

<img src="assets/render-fpstats.png" alt="render-fpstats" style="zoom:50%;" />