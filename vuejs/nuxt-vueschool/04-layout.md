# 04 Layout

Pour ajouter un composant qui apparaît sur toutes les pages, on a le `layout`.

On a `components/NavApp.vue`

```vue
<template>
  <nav>
      <a href="#">Home</a>
      <a href="#">Post</a>
  </nav>
</template>

<script>
export default {
}
</script>
```

On peut alors s'en servir dans le layout :

`layouts/default.vue`

```vue
<template>

  <div>
    <NavApp />
    <Nuxt />
  </div>
</template>

<script>
import NavApp from '~/components/NavApp.vue'

export default {
  components: {
    NavApp
  }
  
}
</script>
```

Les pages sont appelées avec le `component` : `Nuxt`.