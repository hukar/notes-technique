# 10 Les expressions arithmétiques

## Command expansion $( … )

## Arithmetic expansion $(( ... ))

```bash
echo $(( 5*6 ))
30
echo "5 * 6 $((5*6))"
5 * 6 30
```

## Puissance `**`

```bash
echo $((9**3))
729
```

## Les calculs

```bash
n1=10
n2=20

echo " SUM $(( n1 + n2 ))"
echo " PRODUCT $(( n1 * n2 ))"
echo " DIVISION $(( n2 / n1 ))" # division entière ?
echo " REMINDER $(( n2 % 3 ))"
echo " POWER $(( n2 ** 3 ))"
```

## Incrément Décrément

```bash
echo " INCREASE A VARIABLE "

echo " variable is $n1 "
echo " $(( n1-- )) "
echo " variable is $n1 "

echo " INCREASE OF 1 A VARIABLE BEFORE PRINTING"
echo " variable is $n1 "
echo " $(( ++n1 )) "
echo " variable is $n1 "
```

```bash
 INCREASE A VARIABLE 
 variable is 10 
 10 
 variable is 9 
 INCREASE OF 1 A VARIABLE BEFORE PRINTING
 variable is 9 
 10 
 variable is 10 
```

## Raccourci

```bash
echo " SHORT WAY OF OPERATING ON A VARIABLE "

n1=6
n1=$(( n1 + 3 ))
echo " value= $n1 " # 9

echo " ADD $(( n1+=3 )) " # 12
echo " SUBSTRACT $(( n1-=3 )) " # 9
echo " MULTIPLY $(( n1*=3 )) " # 27
echo " DIVIDE $(( n1/=3 )) " # 9
```

