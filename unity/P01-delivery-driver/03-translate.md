# `Translate`

## `transform.Translate()`

```cs
transform.Translate(0, 0.05f, 0);
```



### Tourner en rond : `rotation` + `translation`

<img src="assets/translation-plus-rotation-turn-around.png" alt="translation-plus-rotation-turn-around" style="zoom:50%;" />

```cs
void Update()
{
  transform.Rotate(0, 0, 0.5f);
  transform.Translate(0, 0.05f, 0);
}
```

