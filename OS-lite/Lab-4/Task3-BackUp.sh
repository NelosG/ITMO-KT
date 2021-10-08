#!/bin/bash

CurDate=$(date +%F)
CurDateSeconds=$(date +%s)
# 7 days = 604800 seconds
PathToDir="$HOME/Backup-$CurDate"
PathToSource="$HOME/source"
PathToBackupLog="$HOME/.backup-report.log"
isHomeHasBackup="" #false

for Line in $(echo $(ls "$HOME" | grep "Backup-"))
do
    Date=$(echo $Line | awk -F "Backup-" '{ print $2 }')
    DateSeconds=$(date -j -f '%Y-%m-%d' $Date +%s)
    Delta=$(( $CurDateSeconds - DateSeconds ))
    if [[ 604800 -ge "$Delta" ]]; then
        PathToDir="$HOME/Backup-$Date"
        isHomeHasBackup="true"
        break
    fi
done

if [ $isHomeHasBackup ];
then
    for File in $PathToSource/*
    do
      
        f="$PathToDir/$(basename $File)"
        echo $f
        if [[ -f "$f" ]];
        then
            size1=$(stat -f %z "$File")
            size2=$(stat -f %z "$f")
            echo $size1
            echo $size2
            if [[ $size1 -ne $size2 ]];
            then
                mv "$f" $f.$CurDate
                cp "$File" "$PathToDir"
                echo "$File has been copied" >> "$PathToBackupLog"
                echo "$f -> $f.$CurDate" >> "$PathToBackupLog"
            fi
        else
            cp "$File" "$PathToDir"
            echo "$File has been copied" >> $PathToBackupLog
        fi
    done
else
    mkdir "$HOME/Backup-$CurDate"
    echo "$HOME does not have Backup"
    echo "Backup-$CurDate has been created"
    echo "Backup-$CurDate has been created" >> $PathToBackupLog
    
    for File in $PathToSource/*
    do
        cp "$File" "$PathToDir"
        echo "$File has been copied" >> $PathToBackupLog
    done
fi
