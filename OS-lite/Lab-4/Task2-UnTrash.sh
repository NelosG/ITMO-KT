#!/bin/bash

PathToTrashLog="$HOME/.trash.log"
PathToTrash="$HOME/.trash"
declare -a ArrayOfResultFirst
declare -a ArrayOfResultSecond

if ! [ -d "$PathToTrash" ];
then
    echo "No such directory: $PathToTrash"
    exit 0
fi

if ! [ -f "$PathToTrashLog" ];
then
    echo "No such file: $PathToTrashLog"
    exit 0
fi

echo -e "\n\nAll results:\n"
index=1
while read Line
do
    TMP=$(echo $Line | awk -F ":" '{print $1}' | awk -F "/" '{print $NF}')
    TMP=$(echo "$TMP" | rev | cut -c 2- | rev)
    
    if [[ -n $Line && "$TMP" = "$1" ]]
    then
        ArrayOfResultFirst[index]=$(echo $Line | awk -F ":" '{print $1}')
        
        ArrayOfResultSecond[index]=$(echo $Line | awk -F ":" '{print $2}')
        ArrayOfResultSecond[index]=$(echo "${ArrayOfResultSecond[index]}" | rev | cut -c 1- | rev)
        
        echo "$index) ${ArrayOfResultFirst[index]}"
        
        ArrayOfResultFirst[index]=$(echo "${ArrayOfResultFirst[index]}" | rev | cut -c $((${#1} + 3))- | rev)
        index=$(($index + 1))
    fi
done < $PathToTrashLog

index=$(($index - 1))
echo -e "\nSelect the result you need"
read Number

if [[ "$Number" -lt 1 || "$Number" -gt "$index" ]]
then
    echo "Invalid number"
    exit 0
fi


if ! [ -d "${ArrayOfResultFirst[Number]}" ];
then
    echo -e "\nOriginal directory is not reachable. File will be recover in home directory \n"
    ArrayOfResultFirst[Number]="$HOME"
fi

if [ -f "${ArrayOfResultFirst[Number]}/$1" ];
then
    echo -e "\nFile with the same name is located in the directory. You need to change the file name\n"
    read NewName
    ArrayOfResultFirst[Number]="${ArrayOfResultFirst[Number]}/$NewName"
else
    ArrayOfResultFirst[Number]="${ArrayOfResultFirst[Number]}/$1"
fi

echo ${ArrayOfResultSecond[Number]}
ln ${ArrayOfResultSecond[Number]} "${ArrayOfResultFirst[Number]}"
rm ${ArrayOfResultSecond[Number]}
echo "Success!"
