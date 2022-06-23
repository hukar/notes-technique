# Input prédictif

Côté HTML :

```html
<body>
  <form  name="monForm" action="">

    <input type="text" id="predict">
    <div id="predict-box"></div>
  </form>

  <script type="text/javascript" src="predict.js"></script>
</body>
```

fichier **predict.js**

```javascript
var input = document.getElementById('predict')
var predictBox = document.getElementById('predict-box')
var liTab = predictBox.getElementsByTagName('li')

var predictArr = ["henrie","henriette","hendy","hendouille","chendio","tray","coltrayui","ray","raymond","raymonde"]

var predictStr = predictArr.join(" ")

input.addEventListener('keyup',function () {
  var that = this
  if (this.value!="") {

    var result = "<ul>"
    var regex = new RegExp('\\b('+this.value+')\\w*','g')
    var resultTab = predictStr.match(regex)
    if (resultTab) {

      for (var i = 0;i < resultTab.length;i++) {
        result += "<li>"+resultTab[i]+"</li>"
      }
    }

    result += "</ul>"
    console.log("result : "+result)
    predictBox.innerHTML = result
    for (var i = 0;i < liTab.length;i++) {
      liTab[i].addEventListener('click',function () {
        that.value = this.innerText
        predictBox.innerHTML = ""
      })
    }
  } else {
    predictBox.innerHTML = ""
  }
})
```

### getElementsByTagName

La liste retournée est *live*, c'est à dire qu'elle se met à jour automatiquement à chaque changement de l'arbre DOM. C'est magique !!

### var regex = new RegExp('\\b('+this.value+')\\w*','g')
Cette syntaxe me permet de concatener une variable dans mon expression régulière
**\b** est un début de mot

### Utilisation de that pour récupérer this

`var that = this`

### On peut utiliser filter plutôt que join


```javascript
// var predictStr = predictArr.join(" ")


var resultTab = predictArr.filter(function (element) {
      return regex.test(element)
    })
```

`regex.test()`renvoie un booléen et filter retire du tableau tout ceux qui ont un résultat de *false*