# 13 `dns`

## `dns.lookup` hostname --> ip

`dns.js`

```js
const dns = require("dns"); // name --> address

dns.lookup("pluralsight.com", (err, address) => {
    console.log(address);
});
```

```bash
node dns.js 
44.227.100.72
```

Utilise `libuv thread` (information donné par l'OS et non pas par le réseau).

## `dns.resolve4`

```js
dns.resolve4("pluralsight.com", (err, address) => {
    console.log(address);
});
```

```bash
node dns.js 
[ '52.32.236.218', '54.186.54.235', '44.227.100.72' ]
```

Renvoie un tableau d'adresse `ip`.

## `dns.resolve`

```js
dns.resolve("pluralsight.com", "MX", (err, address) => {
    console.log(address);
});
```

```bahs
node dns.js 
[
  { exchange: 'aspmx.l.google.com', priority: 1 },
  { exchange: 'alt3.aspmx.l.google.com', priority: 10 },
  { exchange: 'alt4.aspmx.l.google.com', priority: 10 },
  { exchange: 'alt1.aspmx.l.google.com', priority: 5 },
  { exchange: 'alt2.aspmx.l.google.com', priority: 5 }
]
```

Le deuxième argument est le type par défaut `'A'` => `ipv4`.

Ici le type est `"MX"` => `mail exchange`.

Chaque type possède un alias, ici ce serait `.resolveMx`.

## `dns.reverse` ip --> hostname

```js
dns.reverse("35.161.75.227", (err, hostname) => {
    console.log(hostname);
});
```

```bash
node dns.js 
[ 'ec2-35-161-75-227.us-west-2.compute.amazonaws.com' ]
```

