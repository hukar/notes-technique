#! /bin/bash
n1=10
n2=20

echo " SUM $(( n1 + n2 ))"
echo " PRODUCT $(( n1 * n2 ))"
echo " DIVISION $(( n2 / n1 ))"
echo " REMINDER $(( n2 % 3 ))"
echo " POWER $(( n2 ** 3 ))"

echo " ______________ "
echo " INCREASE A VARIABLE "

echo " variable is $n1 "
echo " $(( n1-- )) "
echo " variable is $n1 "

echo " INCREASE OF 1 A VARIABLE BEFORE PRINTING"
echo " variable is $n1 "
echo " $(( ++n1 )) "
echo " variable is $n1 "

echo " ______________ "
echo " SHORT WAY OF OPERATING ON A VARIABLE "

n1=6
n1=$(( n1 + 3 ))
echo " value= $n1 "

echo " ADD $(( n1+=3 )) "
echo " SUBSTRACT $(( n1-=3 )) "
echo " MULTIPLY $(( n1*=3 )) "
echo " DIVIDE $(( n1/=3 )) "

