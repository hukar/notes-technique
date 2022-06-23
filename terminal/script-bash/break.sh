#! /bin/bash
echo "begining of the script"

for i in {"blue","elephant","noodle","rice","danger","radio","robot"}
do
    if [ $i == "danger" ];then
        echo "danger instruction"
        break
    fi
    echo "done : $i"
done

echo "end of the script"