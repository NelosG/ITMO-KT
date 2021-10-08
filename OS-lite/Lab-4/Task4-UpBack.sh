#!/bin/bash

PathToBackup=""
PathToBackupSeconds=0
PathToRestore="$HOME/restore"
mkdir "$PathToRestore"

for File in $(echo $(ls "$HOME" | grep "Backup-"))
do
    Date=$(echo $File | awk -F "Backup-" '{ print $2 }')
    DateSeconds=$(date -j -f '%Y-%m-%d' $Date +%s)
    if [[ "$DateSeconds" -gt "$PathToBackupSeconds" ]];
    then
        PathToBackupSeconds=$DateSeconds
        PathToBackup="$HOME/$File"
    fi
done

if [[ PathToBackup != "" ]]
then
    for File in $(echo $(ls "$PathToBackup" | grep -v -E "[A-Za-z0-9]*.[0-9]{4}-[0-9]{2}-[0-9]{2}"))
    do
        cp "$PathToBackup/$File" "$PathToRestore"
    done
fi
