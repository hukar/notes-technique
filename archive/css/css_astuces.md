# alignement vertical de deux blocs

```css
div.conteneur { 
background:#ccc;
width:400px;
padding:10px; /* espace interne du conteneur */
text-align:center; }

div.bloc { 
padding:10px; /* aération interne des blocs */
margin:0 10px; /* espacement horizontal entre les blocs */
width:100px;
border:1px solid #fff;
display:inline-block;
vertical-align:middle;
text-align:left; /* on rétablit l'alignement du texte */ }
```

Mettre les deux blocs dans un conteneur en `display: inline-block` et `vertical-align: middle` (et non pas center !!)

# aligner un texte et une photo

```html
<td>
  <img src="web/img/image001.jpg">
  <h1>
    Service Public<br>
    Fédéral<br>
    FINANCES
  </h1>
</td>
```
Il faut joure sur le vertical-align de l'image

```css
  img{
      vertical-align: top; // aligne par le haut
      height: 2.2cm;

    }

    h1{
      display: inline-block;
      font-size: 11pt;
      height: 1.6cm;
      margin-left: 1.5mm;
      padding-left: 4mm;
      padding-top: 6mm;
      border-left: 3pt solid #999;
    }
```

# supprimer le resize sur un textarea

```css
textarea{
	resize: none;
}
```

### utiliser un div contenteditable

```html
<div contenteditable>

</div>
```

Fonctionne comme un textarea, il faut récupérer le contenu avec javascript

# fade et less

modifier la transparence

```less
color: fade(@macouleur,15%)
```

# ajuster le contenant sur le contenu

div b dans div a :

```css
div.a{
	display: inline-block;
}
```

La div a prend alors la largeur de son contenu.

# éviter les espaces de inline-block

utiliser `display: flex` pour le parent

```css
.parent{
	display: flex;
}
.enfant{
	// rien de spécial
}
```