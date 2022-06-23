# EE Apache Benchmark : `ab`

## Installer par défaut sur Mac

🧡❤️💛 J'aime mon `Mac`.



## Utilisation

```bash
ab -n 100 -c 10 http://localhost:5000/
```

`-n` nombre de requête

`-c` nombre de requête en parallèle



## Exemple

Test d'un serveur `.net` :

```cs
var builder = WebApplication.CreateBuilder(args);
var app = builder.Build();

app
    .MapGet("/", async (context) => {
        // simulate BAD access DB
        Task.Delay(1000).Wait();

        await context.Response.WriteAsync("Hello World!");
    });

app.Run();
```

```bash
✨ ~ : ab -n 100 -c 10 http://localhost:5116/

Concurrency Level:      10
Time taken for tests:   11.111 seconds
Complete requests:      100
Failed requests:        0
Total transferred:      10400 bytes
Requests per second:    9.00 [#/sec] (mean)
Time per request:       1111.101 [ms] (mean)

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    1   0.2      1       1
Processing:  1001 1004   6.6   1004    1068
Waiting:     1001 1004   6.5   1004    1067
Total:       1002 1005   6.6   1005    1068
```

```cs
await Task.Delay(1000); // au lieu de Task.Delay(1000).Wait();
```

```bashawait Task.Delay(1000);
Time taken for tests:   11.046 seconds

Requests per second:    9.05 [#/sec] (mean)
Time per request:       1104.622 [ms] (mean)
Time per request:       110.462 [ms] (mean, across all concurrent requests)
Transfer rate:          0.92 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    1   0.2      1       1
Processing:  1000 1004   1.9   1003    1008
Waiting:     1000 1003   1.8   1003    1007
Total:       1001 1004   1.9   1004    1008
```



### Deuxième série

```cs
Task.Delay(100).Wait();
```

`Wait` bloque la `thread`.

```bash
ab -n 1000 -c 100 http://localhost:5116/
```

```bash
Time taken for tests:   8.403 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      104000 bytes
HTML transferred:       12000 bytes
Requests per second:    119.00 [#/sec] (mean)
Time per request:       840.320 [ms] (mean)
Time per request:       8.403 [ms] (mean, across all concurrent requests)
Transfer rate:          12.09 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    2   1.0      2       5
Processing:   100  820 2151.2    103    7277
Waiting:      100  820 2151.2    103    7277
Total:        100  822 2151.5    105    7281
```

Le temps d'une requête varie entre `103ms` et `7281ms`.

```bash
Percentage of the requests served within a certain time (ms)
  50%    105
  66%    106
  75%    108
  80%    108
  90%   7265
  95%   7273
  98%   7278
  99%   7279
 100%   7281 (longest request)
```



```cs
await Task.Delay(100);
```

`await` ne bloque pas la `Thread`.

```bash
Time taken for tests:   1.240 seconds
Complete requests:      1000
Failed requests:        0
Total transferred:      104000 bytes
HTML transferred:       12000 bytes
Requests per second:    806.17 [#/sec] (mean)
Time per request:       124.043 [ms] (mean)
Time per request:       1.240 [ms] (mean, across all concurrent requests)
Transfer rate:          81.88 [Kbytes/sec] received

Connection Times (ms)
              min  mean[+/-sd] median   max
Connect:        0    2   1.2      2       5
Processing:   100  104   4.1    103     176
Waiting:      100  104   3.8    103     170
Total:        100  106   4.3    106     176
```

On passe d'une `centaine` de requêtes par seconde à `800`.

Le temps est divisé par `8`.

L'écart entre le temps le plus long et le plus court est de `70ms` !!

```bash
Percentage of the requests served within a certain time (ms)
  50%    106
  66%    107
  75%    108
  80%    109
  90%    112
  95%    115
  98%    116
  99%    117
 100%    176 (longest request)
```

