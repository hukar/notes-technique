# 04 `Serialize Field`

On peut rendre accessible des valeurs directement dans l'interface de `Unity` grâce à l'`attribute` `SerializeField`.



## Exemple

```cs
public class Driver : MonoBehaviour
{
    [SerializeField] float moveSpeed = 0.01f;
    [SerializeField] float steerSpeed = 0.3f;
    
    // ...
```

<img src="assets/serialize-field-in-the-editor.png" alt="serialize-field-in-the-editor" style="zoom:50%;" />

On peut alors modifier les valeurs en temps réel pour tester le comportement lorsqu'on lance le jeu

<img src="assets/real-time-modification-with-serialize-feild.png" alt="real-time-modification-with-serialize-feild" style="zoom:50%;" />

On peut faire varier la valeur en se positionnant sur le nom de la variable et en déplaçant le curseur (souris, pad).

<img src="assets/move-cursor-for-change-value.png" alt="move-cursor-for-change-value" style="zoom:50%;" />

























