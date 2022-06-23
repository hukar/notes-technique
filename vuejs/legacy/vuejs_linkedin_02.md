# Les événements

moteur d'événement `v-on`

On écrit la directive dans le html :

```jsx
<span class="round" v-on:click="clickOnRound()"></span>
```

et on retrouve dans notre composant l'attribut methods avec un 's'

```jsx
<script>
export default {
  name: 'game',
  methods: {
    clickOnRound: function () {
      console.log('click click')
    }
  }
}
</script>
```

On peut ajouter des options sur v-on

```jsx
<form v-hide v-on:submit.prevent="addPlayerName">
```
il y a d'autre modificateurs d'événement
  
* stop
* once
* capture
* self
* prevent

## variable réactive

On doit les déclarer dans l'attribut data du composant

```jsx
data: function () {
	      return {
	        player: '',
	        message: ''
	      }
	    },
updated: function () {
   			this.message = `Bonjour <strong>${this.player}</strong> !`
  		},
```

nouveau hook : `updated` qui correspond au changement d'une variable de data

## traquer une touche

```jsx
<script>
export default {
  name: 'game',
  created: function () {
    document.onkeydown = this.start
  },
  methods: {
    clickOnRound: function () {
      console.log('click click')
    },
    bonus: function (event) {
      console.log('BONUS')
      console.log(event)
    },
    start: function (event) {
      console.log(event)
      if (event.key === 'p') {
        alert('pppppppp')
      }
    }
  }
}
</script>
```

on ajoute un event listener dansle **hook** `created` qui appelle une méthode de notre composant.

# des expressions dans moustache !

```jsx
  <div class="score">
    {{score ? 'Votre score : ' + score : 'pas de score !'}}
  </div>
```

On peut écrire des expressions javascript dans les moustaches (dans une interpolation) de vue.

## chaine de caractère et variables `${}`

```js
clickOnRound: function (event) {
      let element = event.target
      let size = Math.random() * (100 - 10) + 10
      element.style.width = element.style.height = `${size}px`
    },
```

utilisation de quotes penchées __`__

# surveiller une variable `watch`

```jsx
<template>
  <div class="game" ref="game" @click="clickOnInterface">
    <span class="round" ref="round" @click.stop="clickOnRound" @click.stop.alt="bonus"></span>
  </div>
</template>

<script>
export default {
  name: 'game',
  data: function () {
    return {
      click: 0
    }
  },
  created: function () {
    document.onkeydown = this.start
  },
  watch: {
    click: function () {
      this.updated()
    }
  },
  methods: {
    clickOnRound: function (event) {
      this.click++
    },
    clickOnInterface: function (event) {
      this.click++
    },
    bonus: function (event) {
      console.log('BONUS')
    },
    start: function (event) {
      if (event.key === 'Enter') {
        console.log('START')
      }
    },
    updated: function () {
      let element = this.$refs.round
      let size = Math.random() * (100 - 10) + 10
      let left = Math.random() * (60 - 5) + 5
      let top = Math.random() * (60 - 5) + 5

      element.style.width = element.style.height = `${size}px`
      element.style.margin = `${top}% ${left}%`
    }
  }
}
</script>
```
`ref` permet de créer un tableau avec nos éléments référencés  
on a donc `let element = this.$refs.round`

`watch` surveille une variable et lance une fonction quand cette variable change de valeur.

# Jouer avec les classes

```jsx
<form :class="{ hide : player }" @submit.prevent="setPlayer">
  <input type="text" name="player" placeholder="entrez votre nom">
  <button type="submit">envoyer</button>
</form>
```

`:class="{ hide : player }"`    
:class = v-bind:class  
{ hide : player } appliquer la classe hide si player n'est pas vide (false)

## Créer un objet style

```jsx
<span class="round" :style="roundStyle"></span>

data: function () {
	return {
	  click: 0,
	  roundStyle: {
	    height: '50px',
	    width: '50px',
	    margin: '10% 20%'
	  }
	}
},
...
updated: function () {
  let size = Math.random() * (100 - 10) + 10
  let left = Math.random() * (60 - 5) + 5
  let top = Math.random() * (60 - 5) + 5

  this.roundStyle.width = this.roundStyle.height = `${size}px`
  this.roundStyle.margin = `${top}% ${left}%`
}
```

`:style="monObjetStyle"` permet de binder le style d'un élément

## On peut ajouter plusieurs classes dynamiques :

```jsx
<span class="round" :class="{ bonus : bonusActivated,bad : badColor }"></span>
```
# boucle for

```jsx
<p v-for="message in collection">{{message}}</p>

data: function () {
return {
   collection: ['message 1', 'message 2', 'message 3']
}
},
```

# v-if

```jsx
<p v-for="item in collection" v-if="item.type === 'users'">
	{{item.message}} : {{item.id}}
</p>
```

# propiété calculée

```jsx
<p v-for="item in userLogs">
	{{item.message}} : {{item.id}}
</p>

computed: {
    userLogs: function () {
      return this.collection.filter(function (item) {
        return item.type === 'users'
      })
    }
  },
```

# communication entre composant

parent: `#app`
enfants: `#score` `#game`

`#game` emmet un événement en lui passant une valeur :

```jsx
  watch: {
    click: function () {
      this.updateRound()
      this.$emit('score', this.click)
    }
  },
```

le parent crée une data avec la valeur à transmettre :

```jsx
  data: function () {
    return {
      score: 0
    }
  },
```

et rend receptif le template :

```jsx
	<game v-on:score="updateScore"></game>
    <score v-bind:score="score"></score>
```

L'autre enfant récupère la valeur de son attribut :

```jsx
<script>
export default {
  name: 'score',
  props: ['score']
}
</script>
```

## utiliser props pour passer des données
Dans `#app`

```jsx
<player :michel="kiki"></player>

...

data: function () {
	return {
	  kiki: 'hello kity',
	  score: 0
	}
},
```

Dans `#player`

```jsx
<p>{{michel}}</p>

...

  export default {
    name: 'player',
    props: ['michel'],
   ...
  }
```