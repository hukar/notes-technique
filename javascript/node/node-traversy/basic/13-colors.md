# 13 colors

```bash
npm i colors
```

Un package qui permet de colorer la sortie `console`.

Plus simple à prendre en main que `chalk`.

```js
const colors = require("colors");

console.log(
  `Server running in ${process.env.NODE_ENV} mode on port ${PORT}`.brightCyan
    .bgGrey
);
```

<img src="assets/Screenshot2020-05-06at08.58.13.png" alt="Screenshot 2020-05-06 at 08.58.13" style="zoom:25%;" />
