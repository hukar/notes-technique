# Installation

simplement en téléchargeant la librairie en version de dev ou de prod

# vue-cli

installation via npm

```sh
sudo npm install --global vue-cli
# ou
sudo npm i -g vue-cli # en abrégé
```

## créer un projet avec vue-cli

```sh
vue init webpack monprojet
# si déjà dans le répertoire du projet
vue init webpack .
```

il y a plusieurs template __webpack, bower et simple__

ensuite

```sh
npm install
# à laracine du projet
```

## Lancer un serveur de dev

```sh
npm run dev
```

# structure d'un composant

## Game.vue

```jsx
<template>

</template>

<script>
export default {
  name: 'game'
}
</script>

<!-- Add "scoped" attribute to limit CSS to this component only -->
<style scoped>

</style>
```

# cycle de vie

il y a plusieurs "hook" created, mounted, destroyed, ...

Ce sont des point de montage sur lesquelles on peut se cabler.

Par exemple dans un composant :

```jsx
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

Apparement les espaces et les guillemets simple sont obligatoires, vuejs impose son style (comme Go)

## données

On peut attacher les données sur __les hook__ :

```jsx
<template>
  <div class="player">
    {{welcomeMessage}}
  </div>
</template>
<script>
  export default {
    name: 'player',
    created: function () {
      this.welcomeMessage = this.player ? 'Bonjour ' + this.player + ' !' : 'Pas de joueur.'
    },
    mounted: function () {
      console.log('salut mounted')
    }
  }
</script>
```
utilisation de la syntaxe __mustache__ dans le template

On peut utiliser l'attribut __data__

```jsx
<template>
  <div class="score">
    Votre score : {{score}}
  </div>
</template>

<script>
export default {
  name: 'score',
  data: function () {
    return {
      score: 0
    }
  },
  created: function () {
    this.score++
  },
  mounted: function () {
    this.score++
  }
}
</script>
```
À l'affichage le score sear donc de 2

# binding

rendre un attribut classe dynamique :

```jsx
<template>
  <div v-bind:class="playerClass">
	...
  </div>
</template>
<script>
  export default {
    name: 'player',
    created: function () {
      this.playerClass = this.player ? 'player' : 'playerForm'
		...
    },
  }
</script>
```

La valeur de la classe playerClass peut prendre __player__ ou **playerForm**

# Créer une directive v-madirective

On crée la directive border qui prend en binding la couleur de la bordure

```jsx
<template>
  <div> 
    <form>
      <input name="player" placeholder="nom" v-border:#f5c9ab>
      <button>envoyer</button>
    </form>
  </div>
</template>
<script>
  export default {
    name: 'player',
    directives: {
      border: function (el, binding) {
        console.log(binding)
        console.log(el)
        el.style.border = '1px solid ' + binding.arg
        el.style.padding = '8px'
        el.style.color = binding.arg
      }
    }
  }
</script>
```

## let et var

let permet de définir une portée de bloc alors que var a une portée de fonction

## une directive hide

```jsx
<template>
  <div class="player">
    <span v-hide>{{welcomeMessage}}</span>
    <form v-hide>
      ...
    </form>
  </div>
</template>
<script>
  ...
    directives: {
      hide: function (el, binding, vnode) {
        let isForm = vnode.tag === 'form'
        let player = vnode.context.player

        if (isForm) {
          el.style.display = player ? 'none' : 'block'
        } else {
          el.style.display = player ? 'block' : 'none'
        }
      }
    }  
  ...
</script>
```

vnode permet de récupérer le context de notre composant et le nom de l'élément sur lequel la directive est appliquée.

# HTML dans une directive

```jsx
this.message = this.player ?
					'Bonjour <strong>' + this.player + '</strong> !' :
					'\\(°-^)/'
```

affiche `Bonjour <strong>bobo</strong> !`

Il faut utiliser les guillemets penchés que les balises soit traitées en HTML et aussi `${}`

```jsx
this.message = this.player ?
 				  `Bonjour <strong>${this.player}</strong> !` :
  				  '\\(°-^)/'
```

ce n'est pas tout il faut utiliser la directive `v-html`

```jsx
<span v-hide v-html="message"></span>
```