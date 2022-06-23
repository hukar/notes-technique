# 03 Ajouter une `method`

Avec la syntaxe normale on fait comme ceci :

```js
methods: {
    increaseCapacity() {
        this.capacity++
    }
}
```

Avec `Composition API` on déclare simplement une fonction dans `setup`.

```vue
<template>
	<div>
        <p>
            Capacity: {{ capacity }}
    	</p>
        <button @click="increaseCapacity()">
            Increase Capacity
    	</button>
    </div>
</template>

<script>
	import { ref } from "vue"
    
    export default {
        setup() {
            const capacity = ref(3)
            
            function increaseCapacity() {
                // ...
            }
            
            return { capacity, increaseCapacity }
        }
    }
</script>
```

La question est comment accéder à la valeur de la `reactive reference`.

Avec la propriété `value` :

```js
function increaseCapacity() {
    capacity.value++
}
```



