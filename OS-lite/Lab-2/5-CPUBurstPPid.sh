#!/bin/bash

PPid="0 "
ARTSum=0
ARTCount=0
>CPUBurstPPid.out
while read line
do
    curPPid=$(echo $line | awk -F ':' '{print $2}'| awk -F '=' '{print $2}')
    curART=$(echo $line | awk -F ':' '{print $3}'| awk -F '=' '{print $2}')
    if [ "$curPPid" != "$PPid" ]
    then
        echo "Average_Sleeping_Children_of_ParentID=" $PPid " is " $(echo "$ARTSum/$ARTCount" | bc -l) >> CPUBurstPPid.out
        PPid=$curPPid
        ARTSum=0
        ARTCount=0
    fi
    ARTSum=$(echo "$ARTSum+$curART" | bc -l)
    ARTCount=$(($ARTCount + 1))
    echo $line >> CPUBurstPPid.out
done < CPUBurst.out
