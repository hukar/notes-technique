## centrer en CSS

Pour centrer du texte verticalement et horizontalement :

```css
.text-box{
    text-align: center;
    width: 100vh;
    min-height: 100px;
    display: table-cell;
    vertical-align: center;
  }
```
__vh__ et __vw__ sont les unités du périphérique "viewport"
Ils résolvent beaucoup de problème
Voici le __html__ :

```html
<div class="box text-box">
  <span>texte<br>texte</span>
</div>
```

Et le résultat :

![](Capture d’écran 2016-12-18 à 16.44.53.png)
 
### centrer une boite dans une boite

En alignement horizontal :

```css
  .big-box{
    height: 300px;
    width: 100vh;
  }
  .small-box{
    height: 100px;
    width: 160px;
    margin-left: auto;
    margin-right: auto;
    text-align: center;
  }
```

On obtient le centrage assez facilement soit avec text-align pour les élément texte soit avec margin-auto pour les boîtes

![](Capture d’écran 2016-12-18 à 16.59.12.png)

## aligner des boîtes flotantes
Il faut enchevétrer à trois niveau
 
1. une boite contenante en `text-align: center`
2. une boite regroupante en `display: inline-block`
3. et puis nos boite en `float: left`
 