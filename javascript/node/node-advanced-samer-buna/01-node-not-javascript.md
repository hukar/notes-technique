# 01 `Node.js` ≠ `javascript`

## Node Architecture

Les deux éléments principaux sont `v8` et `libuv`.

### Les groupes de fonctionnalité de `v8`

`Shipping` en place, utilisable, livré.

`staged` fonctionnalités terminées mais pas forcement stable, nécessite le flag `--harmony`.

`in progress` fonctionnalités à activer individuellement avec son propre flag harmony (doc v8).

### Tester une fonctionnalité

```bash
$ node -p "'Node'.padEnd(8, '*')"
Node****
```

Si cette fonctionnalité n'avait pas marchée, on aurait pu utiliser :

```bash
node --harmony -p # ... staged features
```

### Lister les fonctionnalités `in progress`

```bash
node --v8-options | grep "in progress"
  --harmony-private-methods (enable "harmony private methods in class literals" (in progress))
  --harmony-regexp-sequence (enable "RegExp Unicode sequence properties" (in progress))
  --harmony-weak-refs (enable "harmony weak references" (in progress))
  --harmony-intl-dateformat-quarter (enable "Add quarter option to DateTimeFormat" (in progress))
```

On peut avec le `grep` retrouver des options dans la grande liste :

Pour le `garbage collector` : `gc`

```bash
$ node --v8-options | grep "gc"
  --stress-gc-during-compilation (simulate GC/compiler thread race related to https://crbug.com/v8/8520)
  --wasm-code-gc (enable garbage collection of wasm code)
	# ...
  --expose-gc (expose gc extension)
  # ...
```

### Des informations sur `v8`

```bash
kms: ~ $ node
Welcome to Node.js v12.13.0.
Type ".help" for more information.
> v8
{
  cachedDataVersionTag: [Function: cachedDataVersionTag],
  getHeapSnapshot: [Function: getHeapSnapshot],
  getHeapStatistics: [Function: getHeapStatistics],
  getHeapSpaceStatistics: [Function: getHeapSpaceStatistics],
  getHeapCodeStatistics: [Function: getHeapCodeStatistics],
  setFlagsFromString: [Function: setFlagsFromString],
  Serializer: [Function: Serializer],
  Deserializer: [Function: Deserializer],
  DefaultSerializer: [Function: DefaultSerializer],
  DefaultDeserializer: [Function: DefaultDeserializer],
  deserialize: [Function: deserialize],
  serialize: [Function: serialize],
  writeHeapSnapshot: [Function: writeHeapSnapshot]
}
```
```bash
> v8.getHeapStatistics()
{
  total_heap_size: 5136384,
  total_heap_size_executable: 524288,
  total_physical_size: 2984224,
  total_available_size: 2195583472,
  used_heap_size: 2804144,
  heap_size_limit: 2197815296,
  malloced_memory: 8192,
  peak_malloced_memory: 680224,
  does_zap_garbage: 0,
  number_of_native_contexts: 1,
  number_of_detached_contexts: 0
}
```

![Screenshot 2020-03-09 at 11.32.28](assets/Screenshot 2020-03-09 at 11.32.28.png)

`libuv` s'occupe des communication asynchrone avec les entrées/sorties => file system, tcp/udp.

Elle s'occupe aussi de l'`event-loop`.

### Autres modules importants

`http-parser` parse les requête `http`

`c-ares` => `dns`

`OpenSSl` gère la cryptographie

`zlib` utilisé pour la compression 

