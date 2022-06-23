# 04 Using a debugger

Il faut compiler avec le flag `-g`

## Poser un breakpoint

```bash
lldb ./mon_exe

run

breakpoint set --name myFunction
```

## Avancer

`next`

## Afficher une valeur

```bash
p myVar
```

##Sortir `quit`

