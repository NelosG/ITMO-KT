#!/bin/bash
Num1=$1
Num2=$2
Num3=$3
if [[ $Num1 -ge $Num2 ]]
    then if [[ $Num1 -ge Num3 ]]
        then echo $Num1
        else echo $Num3
    fi
    else if [[ $Num2 -ge Num3 ]]
        then echo $Num2
        else echo $Num3
    fi
fi
exit 0
