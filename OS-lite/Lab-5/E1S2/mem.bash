#!/bin/bash
sleep 5
> report.log

declare -a array
index=0


while true; do
    array+=( "1" "2" "3" "4" "5" "6" "7" "8" "9" "10")

    div=$(($index % 10000))
    
    echo $index
    
    if [[ "$div" -eq "0" && $index -ne "0" ]]; then
        echo $(($index * 10)) >> report.log
    fi

    index=$(($index + 1))
done
