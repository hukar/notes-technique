# BB `Promise` et traitement en parallèle

Pour exécuter des `Promise` en parallèle il faut utiliser `Promise.all` :

```js
function task(nbSeconde) {
    return new Promise((resolve, reject) => {
        console.time(`task ${nbSeconde}`)
        setTimeout(() => {
            console.timeEnd(`task ${nbSeconde}`)
            resolve(nbSeconde)
        }, nbSeconde * 1000)
    })
}
```

```js
async function serialTasks() {
    console.time('serial tasks')

    await task(1)
    await task(2)
    await task(3)
    console.timeEnd('serial tasks')
}

serialTasks()
```

```bash
node parallel-tasks.js 

task 1: 1.003s
task 2: 2.005s
task 3: 3.000s
serial tasks: 6.041s
```

Avec `await` chaque `task` attend que la précédente soit terminée avant de se déclencher.

```js
async function parallelTasks() {
    console.time('promise all')
    await Promise.all([task(1), task(2), task(3)])
    console.timeEnd('promise all')
}
parallelTasks()
```

```bash
node parallel-tasks.js 
task 1: 1.002s
task 2: 2.001s
task 3: 3.002s
promise all: 3.003s
```

Toutes les `tasks` partent en même temps, le temps total est donc égal à la `task` la plus longue.

Ce qui est équivalent à :

```js
task(1.1)
task(2.1)
task(3.1)
parallelTasks()
```

```bash
task 1: 1.002s
task 1.1: 1.105s
task 2: 2.003s
task 2.1: 2.105s
task 3: 3.002s
promise all: 3.002s
task 3.1: 3.105s
```

Les six `tasks` sont exécutée en parallèle.