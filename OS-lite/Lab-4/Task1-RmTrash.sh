#!/bin/bash

PathToTrash="$HOME/.trash"
PathToFile="$PWD/$1"
PathToTrashLog="$HOME/.trash.log"
Date=$(date +%d.%m.%Y-%H%M%S)
 
if ! [ -f "$PathToFile" ];
then
    echo "$PathToFile: No such file"
    exit 0
fi
 
if ! [ -d "$PathToTrash" ];
then
    mkdir "$PathToTrash"
fi

if ! [ -f "$PathToTrashLog" ];
then
    > "$PathToTrashLog"
fi

echo "$PathToFile : $PathToTrash/$Date-Deleted" >> "$PathToTrashLog"
ln "$PathToFile" "$PathToTrash/$Date-Deleted"
rm "$PathToFile"
