# 13 Lecture et écriture dans la console

```js
const readline = require("readline");

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

rl.question("please write me something :", answer => {
    console.log(`you write to me this ${answer}`);

    rl.close();
});
```

`process.stdin` et `process.stdout` sont des streams.

## Illustration simple

```js
process.stdin.on("data", data => {
    process.stdout.write(`\nhello ${data}`);
});
```

```bash
bob

hello bob
```

