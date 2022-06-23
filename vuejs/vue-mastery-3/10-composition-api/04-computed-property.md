# 04 `Computed Property`

```vue
<template>
	<div>
        <p>Space left: {{ spacesLeft }} out of {{ capacity }}</p>
        <button @click="increaseCapacity()">Increase Capacity</button>
        <h2>Attending</h2>
        <ul>
           <li v-for="(name, index) in attending" :key="index">
    			{{ name }}
    		</li> 
    	</ul>
    </div>
</template>

<script>
	import { ref } from "vue"
    
    export default {
        setup() {
            const capacity = ref(3)
            const attending = ref(["Tim", "Bob", "Joe"])
            function increaseCapacity() { capacity.value++ }
            
            return { capacity, increaseCapacity, attending }
        }
    }
</script>
```

Avec l'ancienne syntaxe, on aurait :

```js
computed: {
    spacesLeft() {
        return this.capacity 
    }
}
```

On doit importer `computed` :

```vue
<template>
	<div>
        <p>Space left: {{ spacesLeft }} out of {{ capacity }}</p>
        ...
    </div>
</template>

<script>
	import { ref, computed } from "vue"
    
    export default {
        setup() {
            const capacity = ref(3)
            const attending = ref(["Tim", "Bob", "Joe"])
            const spacesLeft = computed(() => capacity.value - attending.value.length)
            function increaseCapacity() { capacity.value++ }
            
            return { capacity, increaseCapacity, attending, spacesLeft }
        }
    }
</script>
```

