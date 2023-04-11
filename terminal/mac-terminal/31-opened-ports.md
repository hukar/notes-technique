# 31 Vérifier si un `port` est ouvert

## `lsof`

```bash
❯ lsof -i :5238
```

```
COMMAND   PID  USER   FD   TYPE             DEVICE SIZE/OFF NODE NAME
API     72672 hukar  239u  IPv4 0x690f4611a650ec0b      0t0  TCP localhost:5238 (LISTEN)
API     72672 hukar  240u  IPv6 0x690f4608180b1803      0t0  TCP localhost:5238 (LISTEN)
```

Une fois le `PID` obtenu on peut *killer* le processus via `Activity Monitor` par exemple.