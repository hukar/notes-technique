# 14 Capacité des disques

## `df`

`df` disk free: valeur d'espace disponible

`-h` format lisible pour l'homme

```bash
kms: ~ $ df -h
Filesystem      Size   Used  Avail Capacity iused               ifree %iused  Mounted on
/dev/disk1s1   466Gi   39Gi  424Gi     9% 1047755 9223372036853728052    0%   /
devfs          186Ki  186Ki    0Bi   100%     642                   0  100%   /dev
```

## `du`

`du` disk usage : estimation taille d'un fichier ou d'un répertoire.