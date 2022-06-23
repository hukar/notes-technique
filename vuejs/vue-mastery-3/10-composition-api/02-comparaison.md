# 02 Comparaison entre la syntaxe classique et la nouvelle

## Syntaxe classique

```vue
<template>
	<div>
        Capacity : {{ capacity }}
    </div>
</template>

<script>
	export default {
        data() {
            return {
                capacity: 3
            }
        }
    }
</script>
```



## Nouvelle syntaxe

<img src="assets/new-syntax-composition-api.png" alt="new-syntax-composition-api" style="zoom:67%;" />

`setup` s'exécute avant `components`, `props`, `data`, `methods`, `computed` et les `lifecycle methods`.

Il n'y a pas d'accès à `this` dans `setup`.

`setup` a deux arguments optionnels :

### `props`

<img src="assets/props-optionnal-argument.png" alt="props-optionnal-argument" style="zoom:67%;" />

C'est un élément `reactive` et il peut être `watché`.



### `context`

<img src="assets/context-argument-access-this-props.png" alt="context-argument-access-this-props" style="zoom:67%;" />

C'est l'équivalent de `this`, il permet d'accéder à divers propriétés.



### Reactive Reference

On va utiliser (créer) des références réactive avec `ref(value)`.

Cela encapsule notre valeur primitive dans un objet capable de surveiller les changements.

<img src="assets/reactive-refernce.png" alt="reactive-refernce" style="zoom:67%;" />

Finalement `setup` retourne les données devant être accessible dans le template.

```vue
<template>
	<div>
        Capacity : {{ capacity }}
    </div>
</template>

<script>
    import { ref } from "vue"
	export default {
        setup() {
            const capacity = ref(3)
            return { capacity }
        }
    }
</script>
```





















