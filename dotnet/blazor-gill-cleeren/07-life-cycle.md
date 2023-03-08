# 07 `Life Cycle`

On utilise un `override` pour ajouter notre `code` aux événement du `Cycle de vie`.

## Les événements important du `Cycle de vie` d'un `composant`

### `OnInitialized` et `OnInitializedAsync`

C'est le premier événement déclenché lorsque le `composant` s'initialise.

C'est ici que les données sont mise en place pour le `composant`.

C'est aussi ici que les `call` vers les `API` externe ont lieu.



### `OnParametersSet` et `OnParametersSetAsync`

A lieu après `OnInitialized` lorsqu'une nouvelle valeur pour un `paramètre` est reçue.



### `OnAfterRender` et `OnAfterRenderAsync`

À ce point le `composant` est prêt, il a été créé.

Si du code `Javascript` doit interagir avec le `composant`, c'est ici car le `Dom` du `composant` est disponible (pas avant).