# 02 Axios

```js
import axios from "axios";

(async () => {
    const resp = await axios.get(url);
    console.log(resp);
})();

// or

axios.get(url).then((resp) => {
    console.log(resp.data);
})
```

