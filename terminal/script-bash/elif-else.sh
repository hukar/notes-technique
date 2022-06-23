read -p "donner votre âge " age

if [ $age -lt 0 ];then
    echo "vous n'êtes pas encore né"
elif [ $age -ge 0 -a $age -lt 18 ]; then
    echo "vous êtes né mais pas majeur"
elif [ $age -ge 18 -a $age -lt 150 ]; then
    echo "vous êtes un majeur de $age ans"
else
    echo "menteur !!"
fi