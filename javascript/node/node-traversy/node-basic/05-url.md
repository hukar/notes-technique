## 05 Parser l'url : `url`

```js
const url = require("url");

const myUrl = new URL("http://mywebsite.com:8989/hello.html?id=101&name=titi");

// serialized url
console.log(myUrl.href);
console.log(myUrl.toString());
```

```bash
http://mywebsite.com:8989/hello.html?id=101&name=titi
http://mywebsite.com:8989/hello.html?id=101&name=titi
```

#### ! `URL` est en majuscule pour le constructeur et minuscule pour le module

### L'objet `myUrl`

```js
// object
console.log(myUrl);
```

```bash
URL {
  href: 'http://mywebsite.com:8989/hello.html?id=101&name=titi',
  origin: 'http://mywebsite.com:8989',
  protocol: 'http:',
  username: '',
  password: '',
  host: 'mywebsite.com:8989',
  hostname: 'mywebsite.com',
  port: '8989',
  pathname: '/hello.html',
  search: '?id=101&name=titi',
  searchParams: URLSearchParams { 'id' => '101', 'name' => 'titi' },
  hash: ''
}
```

### Les paramètres `searchParams`

```js
console.log(myUrl.searchParams);

// add parameter
myUrl.searchParams.append("abc", "123");
console.log(myUrl.searchParams);

// Loop throught parameters
myUrl.searchParams.forEach((key, value) => {
    console.log(`key : ${key}, value : ${value}`);
});
```

```bash
URLSearchParams { 'id' => '101', 'name' => 'titi' }
URLSearchParams { 'id' => '101', 'name' => 'titi', 'abc' => '123' }
key : 101, value : id
key : titi, value : name
key : 123, value : abc
```

