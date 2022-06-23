# 01 Startup 2 avec Vue js

```bash
sudo npm i -g @vue/cli
```

```bash
vue --version # check the version

sudo npm update -g @vue/cli
```

```bash
vue create-app .

vue add vuetify
```

## Helper de visibilité

Pour afficher un élément seulement à partir d'un breakpoint :

```vue
<span class="hidden-sm-and-down">Documentation</span> 
```



## Router

### Lien : `router-linkl`

```vue
<router-link :to="{ name: 'Home'}">
  Go to Home
</router-link>
```



### fenêtre du `router` : `router-view`

```vue
<v-main>
  <router-view>

  </router-view>
</v-main>
```

