# 02 `Rotate`

## Rotation

### Axe de rotation

<img src="assets/global-and-local.png" alt="global-and-local" style="zoom:50%;" />

La rotation peut être `Global` (les axes `Y` et `X` reste fixe) ou `Local`

<img src="assets/local-rotation.png" alt="local-rotation" style="zoom:50%;" />

Les axes tournent aussi.

Dans la version actuelle c'est `Local` par défaut.

<img src="assets/local-by-default-rotate.png" alt="local-by-default-rotate" style="zoom:50%;" />



### Rotation dans le `Script` : `transform.Rotate`

```cs
transform.Rotate(x, y, z);
```

```cs
void Start()
{
  transform.Rotate(0, 0, 45);
}
```

On peut aussi jouer sur les trois paramètres :

```cs
void Update()
{
  // f for float type      
  transform.Rotate(1, 1.5f, 2);
}
```

<img src="assets/turning-skate-xyz.png" alt="turning-skate-xyz" style="zoom:50%;" />
