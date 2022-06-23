# 04 Les cycles de vie

Mise à disposition de `hook` sur lesquelles on peut se "cabler".

```js
  export default {
    name: 'player',
    mounted: function () {
      console.log('salut mounted')
    },
    created: function () {
      console.log('salut created')
    }
  }
// et on obtient dans la console :
> salut created
> salut mounted
```



## `mounted`





## `created`





## `updated`

Changement d'une propriété réactive.