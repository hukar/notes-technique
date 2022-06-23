# 03 Conditions

```bash
if [ condition ]
then 	
	cmd1
	cmd2
fi
```

ou bien

```bash
if [ consdition ]; then

fi
```

## `elif`  et `else`

Structure entière :

```bash
read -p "ecrire un nombre " nb

if [ $nb -eq 99 ]
then
    echo "bravo !!"
elif [ $nb -lt 99 ]
then
    echo "trop petit"
elif [ $nb -gt 99 -a $nb -le 1000 ]
then
    echo "trop grand"
else
    echo "beaucoup trop grand"
fi
```

#### ! `then`  pour `elif`  mais pas pour ` else`  

