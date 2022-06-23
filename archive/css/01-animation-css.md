# 01 Animation Css

## transition couleur

```css
.btn{
    transition: <property> <duration> <ease> <delay>;
}
```

valeur par defaut :

```css
.btn{
   transition: all <duration> ease 0;
}
```

`transition: all 0.4s ease 0;` == `transition: 0.4s;`

## un effet de slide (glisisière)

```html
<button>
    <span class="top content">
        hello kitty
    </span>
    <span class="bottom content">
        hello coco
    </span>
</button>
```

```css
button{
  font-size: 18px;
  padding: 4px 8px;
  border: 0;
  background-color: green;
  color: gray;
  border-radius: 4px;
  overflow: hidden;
}
button, .content{
  display: block;
  position: relative;
}
.top{
  top: 12px;
  transition: 1s;
}
.bottom{
  bottom: -100px;
  transition: 1s;
}
button:hover .top{
  top: -100px;
}
button:hover .bottom{
  bottom: 12px;
}
```

On joue avec `top` et `bottom` , mettre tout le monde à `display: block` et `position: relative`

## Visibilité

On modifie `opacity` et `visibility`

`Display: none` retire l'élément du DOM et ne fonctionne pas avec les transitions

Pour éffacer un élément on fait :

```css
.hide{
    opacity: 0;
    visibility: hidden;
}
```

Faire un overlay :

```css
.overlay{
  position: fixed;
  background-color: gray;
  top: 0;
  bottom: 0;
  right: 0;
  left: 0;
  z-index: 1040;
  opacity: 1;
  transition: all 1s;
  
}
.overlay.hide{
  visibility: hidden;
  opacity: 0;
}
.modal{
  position: fixed;
  z-index: 1060;
  opacity: 1;
  transition: all 1s;  
}
.modal.hide{
  visibility: hidden;
  opacity: 0;
}
```

`top: 0` `bottom: 0` `right: 0` `left: 0` étire l'overlay sur toute la surface de la fenêtre

`visibility: hidden` pour pouvoir cliquer à travers