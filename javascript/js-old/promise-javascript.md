# Les *promise* en javascript

```js
var p = function(num) {
    return new Promise((resolve, reject) => {

        var n = Math.floor(Math.random()*10);
        
        (n + num)%2 === 0 ? resolve(num + n) : reject((num + n)+" not even");
    })
}


var ps = Promise.all([
    p(4),
    p(7),
    p(3)

])

ps.then(
    (data)=>{
        console.log('valeur de n '+data)
    }).catch((err)=>{
        console.log(err)
    })
```

`Promise.all` traite un tableau de promise, elles doivent toutes être tenues

## Chainer des promises

On peut chainer plusieurs promesses :

```js
p(5).then((data) => {
    console.log('n : ' + data);
    return p(8);  // <- on retourne une nouvelle promesse   
}).then((data) => {
    console.log('n : ' + data);
}).catch((err) => {
    console.log(err);
    
});
```

## Promise 2.0

### ancienne méthode: les callback

```js
function doSomething(successCallback, failureCallback) {
    const n = new Date().getTime();

    const last = n.toString(10).split('').pop();
    if(last % 2 === 0) {
        successCallback(last)
    } else {
        failureCallback('nombre impaire ' + last)
    }
}

function successCallback(data) {
    console.log('on a reçu le nombre ' + data);
}

function failureCallback(error) {
    console.error('affichage du message d\'erreur : ' + error);
}

doSomething(successCallback, failureCallback);
```

Avec une Promise :

```js
function doSomethingElse() {
    return new Promise((success, echec) => {
        const n = new Date().getTime();

        const last = n.toString(10).split('').pop();
        if(last % 2 === 0) {
        success(last)
    } else {
        echec('nombre impaire ' + last)
    }
    })
}

doSomethingElse().then(successCallback, failureCallback);
```

`success` et `echec` s'appellent traditionnellement `resolve` et `reject`.

Un autre exemple asynchrone :

```js
const wait = ms => new Promise((r,rr) => {
    if(ms % 300 === 0) {
        setTimeout(r,ms);
    } else {
        setTimeout(rr,ms);
    }   
})

wait(1000)
    .then(() => console.log('success'))
    .catch(() => console.log('echec'))
    .then(() => console.log('de toute manière'));
```

```bash
=> Promise {}
echec
de toute manière
```

On voit qu'une `promise` est toujours renvoyée et qu'un `catch` n'arrête pas la chaîne de `promise`.