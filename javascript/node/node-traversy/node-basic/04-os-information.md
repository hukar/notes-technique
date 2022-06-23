## 04 Information sur le système d'exploitation `os`

### La plateforme `platform`

```js
const os = require("os");

// Platform
console.log(os.platform());
```

```bash
darwin
```

### L'architecture `arch`

```js
// CPU Arch
console.log(os.arch());
```

```bash
x64
```

### Les cœurs `cpus`

```js
[
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 437100, nice: 0, sys: 237130, idle: 3231710, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 14130, nice: 0, sys: 9470, idle: 3881680, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 331580, nice: 0, sys: 145230, idle: 3428520, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 13440, nice: 0, sys: 7780, idle: 3884070, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 253090, nice: 0, sys: 103910, idle: 3548330, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 13250, nice: 0, sys: 7030, idle: 3885010, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 187280, nice: 0, sys: 70550, idle: 3647490, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 13190, nice: 0, sys: 6230, idle: 3885850, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 154090, nice: 0, sys: 55920, idle: 3695300, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 12970, nice: 0, sys: 5740, idle: 3886570, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 113590, nice: 0, sys: 38450, idle: 3753260, irq: 0 }
  },
  {
    model: 'Intel(R) Core(TM) i7-8850H CPU @ 2.60GHz',
    speed: 2600,
    times: { user: 12970, nice: 0, sys: 5220, idle: 3887070, irq: 0 }
  }
]
```

6 cœurs physiques, 12 virtuels

### La mémoire `freemem` et `totalmem`

```js
// Free memory
console.log(os.freemem());

// Total memory
console.log(os.totalmem());
```

```bash
4503699456
17179869184 # 17179869184 / 1024^3 = 16 GigaBytes
```

Renvoyé en `Bytes`

### Le répertoire home `homedir`

```js
// Home dir
console.log(os.homedir());
```

```bash
/Users/kms
```

### Le temps de fonctionnement `uptime`

```js
// Uptime in seconds
console.log(os.uptime());
```

```bash
7563 # en secondes => 2 h 06 mn
```

> L'**uptime** (en français **durée de fonctionnement**) est un terme [informatique](https://fr.wikipedia.org/wiki/Informatique) désignant le temps depuis lequel une [machine](https://fr.wikipedia.org/wiki/Machine), ou un logiciel informatique, tourne sans interruption. En cas de redémarrage, **l'uptime** est remis à zéro.

## 